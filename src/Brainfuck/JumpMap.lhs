---
title: "Unfancy Haskell pt 3: Brainfuck.JumpMap"
summary: Building a flow-control map from a parsed Brainfuck program.
tags: haskell, static-types, beginner
draft: true
---

> This is part three of a six-part tour of a no-frills [Brainfuck][]
> interpreter in Haskell. The source code for this project is available on
> [github][gh], and each post is written in [literate Haskell][lhs], so you can
> execute these documents directly with GHC.

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

There are exactly two types of jump operations we're concerned with: `[` and
`]`--a jump ahead and a jump back. Here's a data type to represent them:

```haskell
data Jump = Ahead | Back
```

As you may recall from [part one][], we've already defined a `Program` type,
which is a vector of commands. We're enhancing that type here, so let's call the
thing we're constructing a `NumberedProgram`:

``` haskell
type NumberedProgram = [(Index, Command)]
```

Next, we're going to want a list of every jump in the program, in the form of
`(Index, Jump)` pairs. This function iterates through a `NumberedProgram`,
converting `JumpAhead` and `JumpBack` commands into `Ahead` and `Back` jumps,
discarding the rest:

``` haskell
type NumberedJump = (Index, Jump)

onlyJumps :: NumberedProgram -> [NumberedJump]
onlyJumps [] = []
onlyJumps ((idx, JumpAhead):xs) = (idx, Ahead) : onlyJumps xs
onlyJumps ((idx, JumpBack):xs)  = (idx, Back) : onlyJumps xs
onlyJumps (_:xs)                = onlyJumps xs
```

The next function, `jumpMap'` is a real doozy. It does the hard part of building
a jump map: figuring out which `[` goes with which `]`. It uses a stack to keep
track of open `[`s, popping them off when it reaches a `]`:

``` haskell
jumpMap' :: JumpMap -> [NumberedJump] -> [NumberedJump] -> JumpMap
jumpMap' acc []                   []                  = acc
jumpMap' acc stack                (j@(_,Ahead):jumps) = jumpMap' acc (j:stack) jumps
jumpMap' acc ((aidx,Ahead):stack) ((bidx,Back):jumps) = jumpMap' (Map.insert aidx bidx acc) stack jumps
```

There are a few error cases to consider as well. Calling `error` like this is
considered... not great. It totally circumvents the type system and generally
makes it difficult to anticipate and recover from errors. That said, it's great
for this use case because:

  1. If we reach one of these errors then there's nothing we can do or want to
     do to recover. It's game over.
  2. Once we pass this point, we know the program is valid and we're done with error cases.

```haskell
jumpMap' _   []                   ((idx,Back):_)      = error $ "no matching '[' for ']' at " ++ show idx ++ "!"
jumpMap' _   ((idx,_):_)          []                  = error $ "no matching ']' for '[' at " ++ show idx ++ "!"
jumpMap' _   _                    _                   = error "pretty sure this is a bad situation, but I don't know how to put it into words."
```

The pseudo-jump map created above only maps from `Ahead`s to `Back`s, but we
need a map that goes both ways. Here's a quick function to make a unidirectional
map bidirectional:

``` haskell
bidirectionalize :: Ord a => Map.Map a a -> Map.Map a a
bidirectionalize m =
  let kvs = Map.toList m
      vks = map (\(k, v) -> (v, k)) kvs
  in Map.fromList (kvs ++ vks)
```

Finally, bring it all together and write a function that builds a `JumpMap` for a given `Program`.

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

## what's next?

We now have a complete, type-safe, and verified-valid Brainfuck program in
memory. We're halfway there! Next, we'll start laying the groundwork for
actually executing a program by building a representation of runtime memory.

[part one]: /posts/2017-11-26-brainfuck-types.html
