# Quick and Dirty Haskell pt 5

> This is part five of a six-part tour of a no-frills [Brainfuck][bf] interpreter in Haskell.
> The source code for this project is available on [github][gh], and each post is
> written in [literate Haskell][lhs], so you can execute these documents directly
> with GHC.

[lhs]: https://wiki.haskell.org/Literate_programming
[gh]: https://github.com/holguinj/brainfuck
[brainfuck]: https://en.wikipedia.org/wiki/Brainfuck

``` haskell
module Brainfuck.State where
```

``` haskell
import qualified Data.Map         as Map
import qualified Data.Vector      as Vec
import           Brainfuck.Memory (Byte, Memory)
import qualified Brainfuck.Memory as Mem
import           Brainfuck.Types  (Command (..), Index (..), Program)
import qualified Brainfuck.JumpMap as JM
```

``` haskell
data State = State
  { memory  :: Memory
  , ptr     :: Byte
  , jumpMap :: JM.JumpMap
  , program :: Program
  , pc      :: Index
  }
```

``` haskell
blank :: State
blank =
  State
    { memory  = Mem.blank
    , ptr     = 0
    , jumpMap = JM.empty
    , program = Vec.fromList [JumpAhead, JumpBack]
    , pc      = Index 0
    }
```

``` haskell
incPtr :: State -> State
incPtr state = state {ptr = ptr state + 1}

decPtr :: State -> State
decPtr state = state {ptr = ptr state - 1}

incPc :: State -> State
incPc state =
  let (Index pc') = pc state
  in state {pc = Index (pc' + 1)}
```

``` haskell
initState :: Program -> State
initState prog =
  let jm = JM.makeJumpMap prog
  in blank {jumpMap = jm, program = prog}
```

``` haskell
getDest :: Index -> State -> Index
getDest idx state =
  case Map.lookup idx (jumpMap state) of
    Just dest -> dest
    Nothing ->
      error $
      "unable to find a match for jump instruction at " ++ show idx ++ "!" -- shouldn't happen
```
