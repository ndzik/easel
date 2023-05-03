{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module BenchProgram where

import Control.Lens
import Graphics.Rendering.OpenGL
  ( BufferObject,
    NumArrayIndices,
    Shader,
    UniformLocation,
    VertexArrayObject,
  )
import Graphics.Rendering.OpenGL.GL (Program)

data BenchProgram = BenchProgram
  { _benchProgramVertexProgram :: Shader,
    _benchProgramFragmentProgram :: Shader,
    _benchProgramProgram :: Program,
    _benchProgramVAO :: VertexArrayObject,
    _benchProgramVEO :: BufferObject,
    _benchProgramLen :: NumArrayIndices,
    _benchProgramDeltaTimeUniform :: UniformLocation
  }

makeFields ''BenchProgram
