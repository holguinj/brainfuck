# Quick and Dirty Haskell pt 2

> This is part two of a five-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
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
import qualified Brainfuck.Types as T
import qualified Data.Maybe      as Maybe
import qualified Data.Vector     as Vec
```

``` haskell
parseChar :: Char -> Maybe T.Command
parseChar c =
  case c of
    '>' -> Just T.IncPtr
    '<' -> Just T.DecPtr
    '+' -> Just T.IncVal
    '-' -> Just T.DecVal
    '.' -> Just T.PrintChar
    ',' -> Just T.ReadChar
    '[' -> Just T.JumpAhead
    ']' -> Just T.JumpBack
    _   -> Nothing
```

There are 8 significant characters in Brainfuck, and anything else is considered a comment.

``` haskell
parseProgram :: String -> T.Program
parseProgram = Vec.fromList . Maybe.mapMaybe parseChar
```
