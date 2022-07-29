{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module GL where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Synchronous
import Control.Exception hiding (Error)
import Control.Monad (forever, void)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Graphics.GLUtil (linkShaderProgram, loadShader, loadShaderBS)
import Graphics.Rendering.OpenGL as GL hiding (Plane)
import Graphics.UI.GLFW as GLFW
import SWB
import System.Exit (exitSuccess)
import System.FilePath

type Renderable = (VertexArrayObject, NumArrayIndices)

type Point = (Float, Float)

data Plane = Plane Point Float Float

data DefTriangle = DefTriangle Point Point Point

class RenderableObject a where
  toVBO :: a -> [Vertex4 Float]

instance RenderableObject Plane where
  toVBO (Plane (x, y) w h) = [p0, p1, p3, p3, p1, p2]
    where
      rw = w / 2
      rh = h / 2
      p0 = Vertex4 (x - rw) (y - rh) 0 1
      p1 = Vertex4 (x + rw) (y - rh) 0 1
      p2 = Vertex4 (x + rw) (y + rh) 0 1
      p3 = Vertex4 (x - rw) (y + rh) 0 1

instance RenderableObject DefTriangle where
  toVBO (DefTriangle (xp0, yp0) (xp1, yp1) (xp2, yp2)) = [p0, p1, p2]
    where
      p0 = Vertex4 xp0 yp0 0 1
      p1 = Vertex4 xp1 yp1 0 1
      p2 = Vertex4 xp2 yp2 0 1

keyPressed :: Chan () -> GLFW.KeyCallback
keyPressed chan win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = signalShutdown chan win
keyPressed chan win GLFW.Key'Q _ GLFW.KeyState'Pressed _ = signalShutdown chan win
keyPressed _ _ _ _ _ _ = return ()

signalShutdown :: Chan () -> GLFW.WindowCloseCallback
signalShutdown chan win = void $ forkIO (writeChan chan ())

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

update :: (VertexArrayObject, NumArrayIndices) -> GLFW.Window -> IO ()
update (vao, sz) win = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]

  bindVertexArrayObject $= Just vao
  drawArrays Triangles 0 sz

  GLFW.swapBuffers win
  forever $ do
    GLFW.pollEvents
    update (vao, sz) win

defaultVertexShaderName :: FilePath
defaultVertexShaderName = "./shader.vert"

defaultFragmentShaderName :: FilePath
defaultFragmentShaderName = "./shader.frag"

initResources :: RenderableObject a => a -> IO ((VertexArrayObject, NumArrayIndices), (Shader, Shader))
initResources obj = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  withArray vs $ \ptr -> bufferData ArrayBuffer $= (sz, ptr, StaticDraw)
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 4 Float 0 (plusPtr nullPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vsp <- tryLoadFSShader defaultVertexShader defaultVertexShaderName VertexShader
  fsp <- tryLoadFSShader defaultFragmentShader defaultFragmentShaderName FragmentShader
  prog <- linkShaderProgram [vsp, fsp]
  currentProgram $= Just prog
  return ((vao, fromIntegral . length $ vs), (vsp, fsp))
  where
    vs = toVBO obj
    sz = fromIntegral $ length vs * sizeOf (head vs)
    tryLoadFSShader def fp st =
      try @IOError (loadShader st fp) >>= \case
        Left _ -> loadShaderBS (takeFileName fp) st def
        Right r -> return r
