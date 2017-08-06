# brainfuck

A quick and dirty brainfuck interpreter in Haskell.

## Usage

Requires [stack](https://haskellstack.org).

Clone this repository, then run 'stack build && stack install'. The `brainfuck` executable will be installed to `~/.local/bin/`.

You can then run any of the included example programs with `brainfuck --file PROGRAM`:

* hello.bf: prints "Hello, World!"
* rot13.bf: reads one line of input at a time and encodes it using Rot13
* mandelbrot.bf: an ASCII mandelbrot fractal viewer (very slow)
* sierpinski.bf: prints Sierpinski triangle
* brainfuck.bf: a brainfuck interpreter. Reads a brainfuck program and its input, separated by '!'.
