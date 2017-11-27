---
title: "Unfancy Haskell pt 4: Brainfuck.Memory"
summary: Implementing Brainfuck's memory operations.
tags: haskell, static-types, beginner
draft: true
---

> This is part four of a six-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

## Brainfuck.Memory

``` haskell
module Brainfuck.Memory where
```

``` haskell
import Brainfuck.Types
import qualified Data.Map as Map
```

``` haskell
type Memory = Map.Map Byte Byte
```

``` haskell
blank :: Memory
blank = Map.empty
```

``` haskell
deref :: Byte -> Memory -> Byte
deref = Map.findWithDefault 0
```

``` haskell
incVal :: Byte -> Memory -> Memory
incVal ptr memory =
  Map.insertWith (+) ptr 1 memory

decVal :: Byte -> Memory -> Memory
decVal ptr memory =
  Map.insertWith (+) ptr (-1) memory
```

``` haskell
setVal :: Byte -> Byte -> Memory -> Memory
setVal ptr val memory =
  Map.insert ptr val memory
```
