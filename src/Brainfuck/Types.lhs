# Quick and Dirty Haskell pt 1

> This is part one of a five-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

## Introduction to the Series

**Brainfuck is a terrible language**. It's designed to be difficult to read,
difficult to reason about, and difficult to write. On the other hand, it's very
easy to implement: the specification is short and straightforward, you can parse
it with nothing, and you only have pointers, bytes, and indexes to keep track of
at run time.

**Haskell is a great language**. It's designed to be easy to read, easy to reason
about, and easy to write. Unfortunately, a lot of Haskell's most "interesting"
applications involve a lot of language extensions, constructs, and libraries
that are incredibly alienating to not only beginners, but mortals in general.

I think that's a real shame, because Haskell has a lot to offer even without
Kleisli composition or profunctor optics. At its best, Haskell offers concise,
readable code with a high degree of correctness.

### prior art

This was my second time implementing a Brainfuck interpreter. The first was in
Clojure, another great functional programming language, as a weekend project
that I took up on a complete whim. A few hours in, I had a project that could
run "Hello, World!" and a few other simple example programs, but it would choke
on anything more complicated than that.

I spent another hour or two adding additional validation to try to figure out
where exactly the program was going wrong and fixed a couple of bugs, but
ultimately it didn't quite get to where I wanted it to before I lost interest.

### success at last

Fast forward a few months to when I'm starting to feel a lot more comfortable in
Haskell, and I decided I'd go for a rematch, but this time I would have one of the
most advanced type systems in the world to help me get it right.

I'll spoil the ending for you: the Haskell implementation took no longer than my
initial Clojure version, but **it worked absolutely perfectly the first time I ran
it**. Not just simple programs, but everything I could throw at the interpreter
ran without a hitch.


I decided to turn the code into a series of posts that would show how the type
system helped me write a correct interpreter for a language I barely understood
so quickly, and that's what you're reading now.

All of the source files in the [repo][gh] are now in [literate Haskell][lhs], so
what you're reading *is* the program, piece by piece. Feel free to clone it, try
it out, and tweak it all you want.

## Brainfuck.Types

Because this is a literate Haskell file, I can't actually skip any code. Here's
how every module starts:

```haskell
module Brainfuck.Types where
```

This particular module, `Brainfuck.Types`, has basic types that I plan to use
throughout the rest of the code. It doesn't contain *all* the types, but it does
contain most of the types you'd expect to see based on the language spec.

It only requires one other module:

```haskell
import qualified Data.Vector as Vec
```

The [Data.Vector][vec] module lets us represent the program as a `Vector`,
rather than a linked list. If you're familiar with Clojure's vector type, this
one is basically the same.

If we stored the program statements in a list, jumping to the millionth
instruction would take at least one million operations--one for every position
in the list that we have to visit to get to our destination. With a vector,
jumping ahead by a million takes no longer than jumping ahead by one.

Other than that, you can mostly treat vectors and lists similarly, but for a few
instances where you'll see calls to `fromList` and `toList` because some other
function only knows how to deal with lists and not vectors.

Next up, the actual representation of Brainfuck statements as a Haskell type:

``` haskell
data Command
  = IncPtr
  | DecPtr
  | IncVal
  | DecVal
  | PrintChar
  | ReadChar
  | JumpAhead
  | JumpBack
  deriving (Show)
```

The commands above correspond to `>`, `<`, `+`, `-`, `.`, `,`, `[`, and `]`, respectively.
With that defined, we can create a type alias for a `Program` that we can use from now on:

``` haskell
type Program = Vec.Vector Command
```

There are only two other types that are relevant at the level of the
specification. First of all, bytes:

``` haskell
type Byte = Int
```

This will be the type of both pointers and their values (since the language
barely distinguishes between them). We could probably represent bytes in a bunch
of other ways, but `Int` seemed the most straightforward to me. This is just a
type alias, so `Byte` and `Int` will be freely interchangeable throughout the
rest of the code.

The only other 'type' (and I use that term loosely) that exists within Brainfuck
is an index into the running program:

``` haskell
newtype Index =
  Index Int
  deriving (Eq, Ord, Show)
```

The only flow control in Brainfuck is `[` (conditional jump ahead) and `]`
(conditional jump back). There's no equivalent of "goto", and thus no way of
turning a notional `Byte` into a notional `Index`.

Unlike `Int` and `Byte`, I **do not** want `Index` to be interchangeable with
anything. I want to take extra care to keep this type separate from the other
`Int`-based types, and that's exactly what `newtype` is for. Now, the only way
to create a new `Index` is explicitly with the `Index` type constructor, e.g.,
`Index 0`. No such ceremony is required (or allowed) for the `Byte` type, so
hopefully that will keep things clearer later on.

Also, because `Index` is a brand-new type, we need to explicitly derive
instances of `Eq` (the equality typeclass), `Ord` (ordered), and `Show` (for
converting an index to a string).

## What's next?

We have everything we need to represent a Brainfuck program in memory, but no
way to read one! Next up is the Brainfuck.Parse module, which will convert a
string to a `Program`.

[vec]: https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html
