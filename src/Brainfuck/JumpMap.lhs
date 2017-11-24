# Quick and Dirty Haskell pt 4

> This is part four of a six-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

## Brainfuck.JumpMap

Beyond parsing, there's a single pre-processing step that we'll need in order to
execute a given program: we need to map out which jump instructions (`[` and
`]`) match with which others.

To take the smallest possible example: given the program `[JumpAhead,
JumpBack]`, we want to know that the `JumpAhead` at index 0 matches the
`JumpBack` at index 1, and vice-versa.

We start with some standard imports:

``` haskell
module Brainfuck.JumpMap where

import           Brainfuck.Types
import qualified Data.Map        as Map
import qualified Data.Vector     as Vec
```

A `JumpMap` is a `Data.Map.Map` (also known as a hash map) with `Index` keys and
`Index` values:

``` haskell
type JumpMap = Map.Map Index Index
```

Even though you and I know that a `JumpMap` is really just a `Map.Map Index
Index`, I'd prefer to keep that fact hidden as an implementation detail. In
other words, other modules in this program should be able to use `JumpMap`s for
all their intended purposes without ever using `Data.Map`s functions directly.

So given that fact and the fact that we want to expose an empty `JumpMap`,
here's a quick alias:

```haskell
empty :: JumpMap
empty = Map.empty
```


```haskell
data Jump = Ahead | Back
```

``` haskell
type NumberedProgram = [(Index, Command)]

type NumberedJump = (Index, Jump)
```

``` haskell
onlyJumps :: NumberedProgram -> [NumberedJump]
onlyJumps [] = []
onlyJumps ((idx, JumpAhead):xs) = (idx, Ahead) : onlyJumps xs
onlyJumps ((idx, JumpBack):xs)  = (idx, Back) : onlyJumps xs
onlyJumps (_:xs)                = onlyJumps xs
```

``` haskell
jumpMap' :: JumpMap -> [NumberedJump] -> [NumberedJump] -> JumpMap
jumpMap' acc []                   []                  = acc
jumpMap' acc stack                (j@(_,Ahead):jumps) = jumpMap' acc (j:stack) jumps
jumpMap' acc ((aidx,Ahead):stack) ((bidx,Back):jumps) = jumpMap' (Map.insert aidx bidx acc) stack jumps
```

```haskell
jumpMap' _   []                   ((idx,Back):_)      = error $ "no matching '[' for ']' at " ++ show idx ++ "!"
jumpMap' _   ((idx,_):_)          []                  = error $ "no matching ']' for '[' at " ++ show idx ++ "!"
jumpMap' _   _                    _                   = error "pretty sure this is a bad situation, but I don't know how to put it into words."
```

``` haskell
bidirectionalize :: Ord a => Map.Map a a -> Map.Map a a
bidirectionalize m =
  let kvs = Map.toList m
      vks = map (\(k, v) -> (v, k)) kvs
  in Map.fromList (kvs ++ vks)
```

``` haskell
makeJumpMap :: Program -> JumpMap
makeJumpMap prog =
  let numbered = zip indexes (Vec.toList prog)
      jumps = onlyJumps numbered
  in bidirectionalize $ jumpMap' Map.empty [] jumps
  where
    indexes :: [Index]
    indexes = map Index ([0 ..] :: [Int])
```
