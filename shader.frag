#version 410 core

in vec2 texCoord;

uniform sampler2D tex;

out vec3 color;

void main() {
	vec4 col = texture2D(tex, texCoord);
	color = vec3(texCoord.x, 0.5, 0.5);
}
