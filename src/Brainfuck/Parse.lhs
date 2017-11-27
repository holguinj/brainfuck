---
title: "Unfancy Haskell pt 2: Brainfuck.Parse"
draft: true
---

> This is part two of a six-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

## Brainfuck.Parse

``` haskell
module Brainfuck.Parse where
```

The purpose of this module is simple and focused: take a `String` and turn it
into a `Program`. Thankfully, parsing Brainfuck code is just barely more
complicated than parsing binary, and requires no error handling.

Even though Haskell has some great monadic parsing libraries, we're not going to
need any of them. Here's our import list:

``` haskell
import           Brainfuck.Types
import qualified Data.Maybe      as Maybe
import qualified Data.Vector     as Vec
```

The core of this module is a single function: `parseChar`. There are 8
significant characters in Brainfuck, and anything else is considered a comment.
Since we just want to discard the comments, we'll represent them as `Nothing` in
the `Maybe Command` type:

``` haskell
parseChar :: Char -> Maybe Command
parseChar c =
  case c of
    '>' -> Just IncPtr
    '<' -> Just DecPtr
    '+' -> Just IncVal
    '-' -> Just DecVal
    '.' -> Just PrintChar
    ',' -> Just ReadChar
    '[' -> Just JumpAhead
    ']' -> Just JumpBack
    _   -> Nothing
```

Now we just need to map over the input string with `parseChar`, discard the
`Nothing`s and unwrap the `Just`s, and return a vector:

``` haskell
parseProgram :: String -> Program
parseProgram = Vec.fromList . Maybe.mapMaybe parseChar
```

`Maybe.mapMaybe` does a lot of the work here for us--it applies `parseChar` to
every character individually and only keeps the `Just` values, unwrapping them
to get the underlying `Command` data. It returns a list, though, so we still
have to call `Vec.fromList` on the result.

But that's it! Pretty much... Well, actually there's one more thing. A Brainfuck
program isn't *really* just a flat list of tokens--every `JumpAhead` command has
to have a corresponding `JumpBack` command (and vice-versa), and we'll need a
quick way to tell which one matches up with which. In the next part of this
series, we'll look at the `Brainfuck.JumpMap` module, which handles building
that map.
