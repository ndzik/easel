# ShaderWorkBench

Simple workbench dynamically (re-)loading your written GLSL shader allowing one to toy around.

# Usage

`cabal run` or build the executable and run it manually.
**SWB** will search for `shader.vert` and `shader.frag` files in the current directory.
If present, SWB will try to compile and load them.
The default shader is a simple red plane.
