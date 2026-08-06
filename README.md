# Backalley
 A breakout clone I wrote in COBOL. Also includes emscripten port.

# How to compile
#### Desktop version

 `cobc -fstatic-call -x -O3 -lSDL2 Backalley.cbl`

#### Web Version
 Well first you need to compile the [GNU Cobol runtime library](https://sourceforge.net/projects/gnucobol/) to WebAssembly. This excercise is left to the reader. Then run the following.

```
cobc -C -fstatic-call Backalley_em.cbl 
emcc -O3 -sUSE_SDL=2 Backalley_em.c -lgmp -lcob -o backalley.html
```
