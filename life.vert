#version 150

uniform vec3 xyz;
uniform mat4 camera;
uniform float cellsPerEdge;
uniform float time;
in vec3 position;
in float weight;
out float Weight;

void main() {
  vec3 h = vec3(1) / 2 + vec3(cellsPerEdge) / 2;
  gl_Position = camera * vec4((position - h + xyz)/cellsPerEdge, 1);
  Weight = weight;
}
