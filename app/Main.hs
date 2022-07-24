{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.ByteString.Char8 (pack)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Graphics.GLUtil (linkShaderProgram, loadShaderBS)
import Graphics.Rendering.OpenGL as GL hiding (Plane)
import Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)

type Point = (Float, Float)

data Plane = Plane Point Float Float

data DefTriangle = DefTriangle Point Point Point

class Renderable a where
  toVBO :: a -> [Vertex4 Float]

instance Renderable Plane where
  toVBO (Plane (x, y) w h) = [p0, p1, p2, p3]
    where
      rw = w / 2
      rh = h / 2
      p0 = Vertex4 (x - rw) (y - rh) 0 1
      p1 = Vertex4 (x + rw) (y - rh) 0 1
      p2 = Vertex4 (x + rw) (y + rh) 0 1
      p3 = Vertex4 (x - rw) (y + rh) 0 1

instance Renderable DefTriangle where
  toVBO (DefTriangle (xp0, yp0) (xp1, yp1) (xp2, yp2)) = [p0, p1, p2]
    where
      p0 = Vertex4 xp0 yp0 0 1
      p1 = Vertex4 xp1 yp1 0 1
      p2 = Vertex4 xp2 yp2 0 1

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed win GLFW.Key'Q _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _ _ _ _ _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
  do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

update :: (VertexArrayObject, BufferObject) -> GLFW.Window -> IO ()
update (vao, vbo) win = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]

  vertexAttribArray (AttribLocation 0) $= Enabled
  bindBuffer ArrayBuffer $= Just vbo
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 4 Float 0 (plusPtr nullPtr 0))
  drawArrays Triangles 0 3
  vertexAttribArray (AttribLocation 0) $= Disabled

  GLFW.swapBuffers win
  forever $ do
    GLFW.pollEvents
    update (vao, vbo) win

initResources :: [Vertex4 Float] -> IO (VertexArrayObject, BufferObject)
initResources vs = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  withArray vs $ \ptr ->
    let sz = fromIntegral $ length vs * sizeOf (head vs)
     in bufferData ArrayBuffer $= (sz, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 4 Float 0 (plusPtr nullPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  vsp <-
    loadShaderBS "vertex_shader" VertexShader $
      pack . unlines $
        [ "#version 330 core",
          "layout(location = 0) in vec3 vertexPosition_modelspace;",
          "void main(){",
          " gl_Position.xyz = vertexPosition_modelspace;",
          " gl_Position.w = 1.0;",
          "}"
        ]

  fsp <-
    loadShaderBS "fragment_shader" FragmentShader $
      pack . unlines $
        [ "#version 330 core",
          "out vec3 color;",
          "void main(){",
          " color = vec3(1,0,0);",
          "}"
        ]

  prog <- linkShaderProgram [vsp, fsp]
  currentProgram $= Just prog

  return (vao, vbo)

-- | init OpenGL and create an OpenGL window
initGL :: Renderable a => a -> IO ()
initGL p = do
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 512 512 "WorkBench" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  (initResources . toVBO) p >>= flip update win
  GLFW.destroyWindow win
  GLFW.terminate

main :: IO ()
main = initGL $ DefTriangle (-0.5, -0.5) (0, 0.5) (0.5, -0.5)
