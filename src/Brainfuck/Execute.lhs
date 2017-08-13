# Quick and Dirty Haskell pt 6

> This is part six of a six-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

``` haskell
module Brainfuck.Execute where
```

``` haskell
import qualified Brainfuck.Memory as Mem
import qualified Brainfuck.Parse  as Parse
import qualified Brainfuck.State  as St
import qualified Brainfuck.Types  as T
import           Data.Char        (chr, ord)
import qualified Data.Vector      as Vec
```

``` haskell
onMemory :: (Mem.Memory -> Mem.Memory) -> St.State -> St.State
onMemory f state = state { St.memory = f (St.memory state) }
```

``` haskell
runCmd :: T.Command -> St.State -> IO St.State
runCmd T.IncPtr state = return (St.incPtr state)
runCmd T.DecPtr state = return (St.decPtr state)
```

First, two simplest cases. These translate quite easily to memory operations, so
we use `onMemory` to apply the `Mem.decVal ptr` function to just the underlying
`Memory` map.

Because we're returning `IO St.State` and not merely `St.State`, we have to call
the `IO` type's `return` function to wrap the result.

``` haskell
runCmd T.IncVal state = do
  let ptr = St.ptr state
  return $ onMemory (Mem.incVal ptr) state
runCmd T.DecVal state = do
  let ptr = St.ptr state
  return $ onMemory (Mem.decVal ptr) state
```

``` haskell
runCmd T.PrintChar state = do
  let ptr = St.ptr state
  let val = Mem.deref ptr (St.memory state)
  putChar (chr val)
  return state
runCmd T.ReadChar state = do
  char <- getChar
  let val = ord char
  let ptr = St.ptr state
  return $ onMemory (Mem.setVal ptr val) state
```

``` haskell
runCmd T.JumpAhead state = do
  let ptr = St.ptr state
  let val = Mem.deref ptr (St.memory state)
  let dest = St.getDest (St.pc state) state
  if val == 0
    then return $ state {St.pc = dest}
    else return state
runCmd T.JumpBack state = do
  let ptr = St.ptr state
  let val = Mem.deref ptr (St.memory state)
  let dest = St.getDest (St.pc state) state
  if val /= 0
    then return $ state {St.pc = dest}
    else return state
```

``` haskell
runProgram' :: St.State -> IO St.State
runProgram' state = do
  let (T.Index idx) = St.pc state
  case St.program state Vec.!? idx of
    Nothing -> return state
    Just cmd -> do
      newState <- runCmd cmd state
      runProgram' (St.incPc newState)

runProgram :: T.Program -> IO ()
runProgram prog = do
  let state = St.initState prog
  _ <- runProgram' state
  return ()
```

``` haskell
runString :: String -> IO ()
runString = runProgram . Parse.parseProgram
```
