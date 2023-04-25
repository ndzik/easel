#version 410 core

layout(location = 0) in vec4 vertexPosition_modelspace;
layout(location = 1) in vec2 iTexCoord;

out vec2 texCoord;

void main() {
	gl_Position = vertexPosition_modelspace;
	texCoord = vec2(iTexCoord.x, iTexCoord.y);
}
