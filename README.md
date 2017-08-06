# brainfuck

A quick and dirty [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter in Haskell.

## Usage

Requires [stack](https://haskellstack.org).

Clone this repository, then run `stack build && stack install`. The `brainfuck` executable will be installed to `~/.local/bin/`.

You can then run any of the included example programs with `brainfuck --file PROGRAM`:

* hello.bf: prints "Hello, World!"
* rot13.bf: reads one line of input at a time and encodes it using Rot13
* mandelbrot.bf: an ASCII mandelbrot fractal viewer (very slow)
* sierpinski.bf: prints Sierpinski triangle
* brainfuck.bf: a brainfuck interpreter. Reads a brainfuck program and its input, separated by '!'.

## Caveats

So far this has been able to run every (valid) brainfuck program that I've tried, but I'd be interested to know if you find one that it doesn't run.
I've only paid minimal attention to performance, so don't expect anything special.
Brainfuck programs can loop indefinitely (and many of them do), so hit `Ctrl+c` to exit if necessary.
