# Quick and Dirty Haskell pt 4

> This is part four of a six-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

``` haskell
module Brainfuck.JumpMap where

import           Brainfuck.Types
import qualified Data.Map        as Map
import qualified Data.Vector     as Vec
```

``` haskell
type JumpMap = Map.Map Index Index

empty :: JumpMap
empty = Map.empty
```

``` haskell
type NumberedProgram = [(Index, Command)]

data Jump = Ahead | Back

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
