{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bench (executeBench) where

import Colog (Severity (..), cmap, fmtMessage, log, logTextStdout, usingLoggerT, withLog)
import Control.Concurrent.Chan.Synchronous
import Control.Exception hiding (Error)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Free
import Control.Monad.Freer.State
import Control.Monad.Freer.TH
import Control.Monad.IO.Class
import Data.Either.Extra (mapLeft)
import Data.Text (Text, pack)
import Foreign.Ptr (nullPtr)
import GL
import Graphics.GLUtil (linkShaderProgram, loadShader)
import Graphics.Rendering.OpenGL (Program, Shader, ShaderType (..), createShader)
import Graphics.Rendering.OpenGL as GL hiding (Error, Plane, get)
import Graphics.UI.GLFW as GLFW
import SWB
import System.Directory (makeAbsolute)
import System.Exit (exitSuccess)
import System.FSNotify
import System.FilePath
import System.IO.Error (IOError)
import Prelude hiding (log)

data UpdateType = VertexUpdate | FragmentUpdate deriving (Show, Eq)

data Logging r where
  LogInfo :: Text -> Logging ()
  LogError :: Text -> Logging ()

makeEffect ''Logging

runColog :: (LastMember IO effs) => Eff (Logging : effs) a -> Eff effs a
runColog = interpretM $ \case
  LogInfo msg -> withColog Info msg
  LogError msg -> withColog Error msg

withColog :: Severity -> Text -> IO ()
withColog s = usingLoggerT (cmap fmtMessage logTextStdout) . log s

newtype ShaderError = ShaderError Text deriving (Show, Eq)

data ShaderLoader s r where
  LoadVertexShader :: s -> ShaderLoader s (Either ShaderError Shader)
  LoadFragmentShader :: s -> ShaderLoader s (Either ShaderError Shader)

makeEffect ''ShaderLoader

runFilesystemShaderLoader ::
  ( Member Logging effs,
    LastMember IO effs
  ) =>
  Eff (ShaderLoader FilePath : effs) a ->
  Eff effs a
runFilesystemShaderLoader = interpret $ \case
  LoadVertexShader fp -> do
    logInfo $ "Loading vertex shader: " <> pack fp
    liftIO $ mapLeft (ShaderError . pack . show) <$> try @IOError (loadShader VertexShader fp)
  LoadFragmentShader fp -> do
    logInfo $ "Loading fragment shader: " <> pack fp
    liftIO $ mapLeft (ShaderError . pack . show) <$> try @IOError (loadShader FragmentShader fp)

data Bench = Bench
  { _vsProg :: !Shader,
    _fsProg :: !Shader,
    _vsPath :: !FilePath,
    _fsPath :: !FilePath,
    _quitChan :: !(Chan ()),
    _updateChan :: !(Chan UpdateType),
    _vao :: !(Maybe VertexArrayObject)
  }

makeLenses ''Bench

data Updater r where
  UpdateFragmentShader :: Updater ()
  UpdateVertexShader :: Updater ()

makeEffect ''Updater

runUpdater ::
  ( Members [ShaderLoader FilePath, Logging, State Bench, IO] effs,
    LastMember IO effs
  ) =>
  Eff (Updater : effs) a ->
  Eff effs a
runUpdater =
  let updateShaders ::
        ( Members [Logging, State Bench, IO] effs,
          LastMember IO effs
        ) =>
        Eff effs ()
      updateShaders = do
        vs <- gets _vsProg
        fs <- gets _fsProg
        logInfo "Trying to link new shader program..."
        prog <- liftIO $ linkShaderProgram [vs, fs]
        logInfo "Linking successful!"
        vao <- gets _vao
        liftIO $ bindVertexArrayObject $= vao
        liftIO $ currentProgram $= Just prog
        logInfo "New shader program loaded"
   in interpret $
        \case
          UpdateFragmentShader ->
            gets _fsPath >>= loadFragmentShader >>= \case
              Left err -> logError . pack . show $ err
              Right s -> do
                logInfo "FragmentShader compilation successful"
                modify (set fsProg s)
                updateShaders
          UpdateVertexShader ->
            gets _vsPath >>= loadVertexShader >>= \case
              Left err -> logError . pack . show $ err
              Right s -> do
                logInfo "VertexShader compilation successful"
                modify (set vsProg s)
                updateShaders

data Initializer w r where
  InitContext :: Initializer w w

makeEffect ''Initializer

runInitializer ::
  ( Members [Logging, State Bench, IO] effs,
    LastMember IO effs,
    RenderableObject o
  ) =>
  o ->
  Eff (Initializer (Window, Renderable) : effs) a ->
  Eff effs a
runInitializer obj runEff = do
  chan <- gets _quitChan
  interpret
    ( \case
        InitContext -> do
          liftIO GLFW.init
          liftIO GLFW.defaultWindowHints
          win <-
            liftIO $
              GLFW.createWindow 512 512 "WorkBench" Nothing Nothing >>= \case
                Nothing -> throwError . userError $ "Failed to create GLFW window"
                Just win -> return win
          liftIO $ GLFW.makeContextCurrent (Just win)
          liftIO $ GLFW.setWindowSizeCallback win (Just resizeWindow)
          liftIO $ GLFW.setKeyCallback win (Just $ keyPressed chan)
          liftIO $ GLFW.setWindowCloseCallback win (Just $ signalShutdown chan)
          (r@(vao', _), (vs, fs)) <- liftIO $ initResources obj
          modify (vsProg .~ vs)
          modify (fsProg .~ fs)
          modify (vao ?~ vao')
          return (win, r)
    )
    runEff

data Quitter w r where
  QuitIt :: w -> Quitter w ()

makeEffect ''Quitter

runQuitter :: (LastMember IO effs) => Eff (Quitter Window : effs) a -> Eff effs a
runQuitter = interpretM $ \case
  QuitIt win -> do
    GLFW.destroyWindow win
    GLFW.terminate
    void exitSuccess

data Listener r where
  Listen :: Chan a -> Listener (Maybe a)

makeEffect ''Listener

runListener :: (LastMember IO effs) => Eff (Listener : effs) a -> Eff effs a
runListener = interpretM $ \case
  Listen c -> maybeTry (tryReadChan c)

data Renderer r where
  RenderIt :: Window -> Renderable -> Renderer ()

makeEffect ''Renderer

runRenderer ::
  ( Members [Logging, Listener] effs,
    LastMember IO effs
  ) =>
  Eff (Renderer : effs) a ->
  Eff effs a
runRenderer = interpretM $ \case
  RenderIt win (vao, sz) -> do
    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer]

    bindVertexArrayObject $= Just vao
    drawElements Triangles 6 UnsignedInt nullPtr

    GLFW.swapBuffers win
    GLFW.pollEvents

type WorkBenchEffects ls iv q =
  [ ShaderLoader ls,
    Logging,
    State Bench,
    Listener,
    Renderer,
    Initializer iv,
    Quitter q,
    Updater
  ]

workbenchProgram ::
  ( Members
      (WorkBenchEffects FilePath (Window, Renderable) Window)
      effs
  ) =>
  Eff effs ()
workbenchProgram = do
  logInfo "Setting up workbench"
  (w, renderable@(vao', _)) <- initContext @(Window, Renderable)
  logInfo "Starting workbench"
  qc <- gets _quitChan
  uc <- gets _updateChan
  let renderUntilDone =
        listen qc >>= \case
          Just _ -> return ()
          Nothing ->
            listen uc >>= \case
              Just VertexUpdate -> updateVertexShader >> renderIt w renderable >> renderUntilDone
              Just FragmentUpdate -> updateFragmentShader >> renderIt w renderable >> renderUntilDone
              Nothing -> renderIt w renderable >> renderUntilDone
  renderUntilDone
  logInfo "Quitting workbench"
  quitIt w

runBench ::
  (RenderableObject o) =>
  o ->
  Bench ->
  Eff
    [ Updater,
      ShaderLoader FilePath,
      Renderer,
      Listener,
      Initializer (Window, Renderable),
      Quitter Window,
      State Bench,
      Logging,
      IO
    ]
    a ->
  IO a
runBench obj defBench =
  runM
    . runColog
    . evalState defBench
    . runQuitter
    . runInitializer obj
    . runListener
    . runRenderer
    . runFilesystemShaderLoader
    . runUpdater

shaderWatcher :: Chan UpdateType -> WatchManager -> IO ()
shaderWatcher chan mgr = do
  void $
    watchTree
      mgr
      "."
      ( \case
          Added {} -> True
          Modified {} -> True
          _else -> False
      )
      ( \case
          Added fp _ _
            | takeFileName fp == takeFileName defaultVertexShaderName -> writeChan chan VertexUpdate
            | takeFileName fp == takeFileName defaultFragmentShaderName -> writeChan chan FragmentUpdate
          Modified fp _ _
            | takeFileName fp == takeFileName defaultVertexShaderName -> writeChan chan VertexUpdate
            | takeFileName fp == takeFileName defaultFragmentShaderName -> writeChan chan FragmentUpdate
          _else -> return ()
      )

executeBench :: IO ()
executeBench = do
  withManager $ \mgr -> do
    b <-
      Bench <$> createShader VertexShader
        <*> createShader FragmentShader
        <*> makeAbsolute defaultVertexShaderName
        <*> makeAbsolute defaultFragmentShaderName
        <*> newChan @()
        <*> newChan @UpdateType
        <*> return Nothing
    shaderWatcher (b ^. updateChan) mgr
    void $ runBench (Plane (0, 0) 1.8 1.8) b workbenchProgram
