# Quick and Dirty Haskell pt 2

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

The purpose of this module is simple and focused: take a `String` and turn it into a `Program`.
In keeping with the quick-and-dirty philosophy, I'm not going to do any error handling here.

``` haskell
import Brainfuck.Types
import qualified Data.Maybe      as Maybe
import qualified Data.Vector     as Vec
```

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

There are 8 significant characters in Brainfuck, and anything else is considered a comment.

``` haskell
parseProgram :: String -> Program
parseProgram = Vec.fromList . Maybe.mapMaybe parseChar
```
