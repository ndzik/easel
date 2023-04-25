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

data Plane = Plane !Point !Float !Float

data DefTriangle = DefTriangle !Point !Point !Point

class RenderableObject a where
  toVBO :: a -> [(Vertex4 Float, Vertex2 Float)]
  toEBO :: a -> [GLuint]

instance RenderableObject Plane where
  toVBO (Plane (x, y) w h) = [(p0, p0_tex), (p1, p1_tex), (p2, p2_tex), (p3, p3_tex)]
    where
      rw = w / 2
      rh = h / 2
      p0 = Vertex4 (x - rw) (y - rh) 0 1
      p0_tex = Vertex2 0 0
      p1 = Vertex4 (x + rw) (y - rh) 0 1
      p1_tex = Vertex2 1 0
      p2 = Vertex4 (x + rw) (y + rh) 0 1
      p2_tex = Vertex2 1 1
      p3 = Vertex4 (x - rw) (y + rh) 0 1
      p3_tex = Vertex2 0 1
  toEBO Plane {} = [0, 1, 2, 0, 2, 3]

instance RenderableObject DefTriangle where
  toVBO (DefTriangle (xp0, yp0) (xp1, yp1) (xp2, yp2)) = [(p0, p0_tex), (p1, p1_tex), (p2, p2_tex)]
    where
      p0 = Vertex4 xp0 yp0 0 1
      p0_tex = Vertex2 0 0
      p1 = Vertex4 xp1 yp1 0 1
      p1_tex = Vertex2 0.5 1
      p2 = Vertex4 xp2 yp2 0 1
      p2_tex = Vertex2 1 0
  toEBO DefTriangle {} = [0, 1, 2]

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
  drawElements Triangles 3 UnsignedInt nullPtr

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
  veo <- genObjectName
  let vertexAttributeLocation = AttribLocation 0
      texAttributeLocation = AttribLocation 1
  -- VAO definition.
  withArray vs $ \ptr -> bufferData ArrayBuffer $= (vssz, ptr, StaticDraw)
  vertexAttribPointer vertexAttributeLocation $= (ToFloat, VertexArrayDescriptor 4 Float stride (plusPtr nullPtr 0))
  vertexAttribArray vertexAttributeLocation $= Enabled
  -- EBO definition.
  bindBuffer ElementArrayBuffer $= Just veo
  vertexAttribPointer texAttributeLocation $= (ToFloat, VertexArrayDescriptor 2 Float stride (plusPtr nullPtr (4 * floatSize)))
  vertexAttribArray texAttributeLocation $= Enabled
  withArray es $ \ptr -> bufferData ElementArrayBuffer $= (essz, ptr, StaticDraw)
  vsp <- tryLoadFSShader defaultVertexShader defaultVertexShaderName VertexShader
  fsp <- tryLoadFSShader defaultFragmentShader defaultFragmentShaderName FragmentShader
  prog <- linkShaderProgram [vsp, fsp]
  currentProgram $= Just prog
  return ((vao, fromIntegral . length $ vs), (vsp, fsp))
  where
    floatSize = sizeOf (0 :: Float)
    stride = fromIntegral $ 6 * floatSize
    flatten :: [Float] -> [(Vertex4 Float, Vertex2 Float)] -> [Float]
    flatten rs [] = reverse rs
    flatten rs ((Vertex4 x1 y1 z1 w1, Vertex2 x2 y2) : xs) = flatten (y2 : x2 : w1 : z1 : y1 : x1 : rs) xs
    vs = flatten [] . toVBO $ obj
    es = toEBO obj
    vssz = fromIntegral $ length vs * sizeOf (head vs)
    essz = fromIntegral $ length es * sizeOf (head es)
    tryLoadFSShader def fp st =
      try @IOError (loadShader st fp) >>= \case
        Left _ -> loadShaderBS (takeFileName fp) st def
        Right r -> return r
