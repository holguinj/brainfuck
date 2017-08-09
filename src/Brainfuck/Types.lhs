# Quick and Dirty Haskell pt 1

> This is part one of a five-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

## Brainfuck.Types

```haskell
module Brainfuck.Types where
```

```haskell
import qualified Data.Vector as Vec
```

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

``` haskell
type Program = Vec.Vector Command
```

``` haskell
newtype Index =
  Index Int
  deriving (Eq, Ord, Show)
```

