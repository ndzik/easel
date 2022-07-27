module SWB where

import Data.ByteString.Char8 hiding (unlines)

defaultVertexShader :: ByteString
defaultVertexShader =
  pack .unlines $
    [ "#version 330 core",
      "layout(location = 0) in vec3 vertexPosition_modelspace;",
      "void main(){",
      " gl_Position.xyz = vertexPosition_modelspace;",
      " gl_Position.w = 1.0;",
      "}"
    ]

defaultFragmentShader :: ByteString
defaultFragmentShader =
  pack . unlines $
    [ "#version 330 core",
      "out vec3 color;",
      "void main(){",
      " color = vec3(1,0,0);",
      "}"
    ]
