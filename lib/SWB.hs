module SWB where

import Data.ByteString.Char8 hiding (unlines)

defaultVertexShader :: ByteString
defaultVertexShader =
  pack . unlines $
    [ "#version 410 core",
      "",
      "layout(location = 0) in vec4 vertexPosition_modelspace;",
      "layout(location = 1) in vec2 iTexCoord;",
      "",
      "out vec2 texCoord;",
      "",
      "void main() {",
      "gl_Position = vertexPosition_modelspace;",
      "texCoord = vec2(iTexCoord.x, iTexCoord.y);",
      "}"
    ]

defaultFragmentShader :: ByteString
defaultFragmentShader =
  pack . unlines $
    [ "#version 410 core",
      "",
      "in vec2 texCoord;",
      "",
      "uniform sampler2D tex;",
      "",
      "out vec3 color;",
      "",
      "void main() {",
      "vec4 col = texture2D(tex, texCoord);",
      "color = vec3(texCoord.x, 0.5, 0.5);",
      "}"
    ]
