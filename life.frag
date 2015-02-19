#version 150

in float Weight;
out vec4 outColor;

void main()
{
  outColor = vec4(Weight, Weight, Weight, 1);
}
