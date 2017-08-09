module Brainfuck.State where

import qualified Data.Map         as Map
import qualified Data.Vector      as Vec
import           Brainfuck.Memory (Byte, Memory)
import qualified Brainfuck.Memory as Mem
import           Brainfuck.Types  (Command (..), Index (..), Program)
import qualified Brainfuck.JumpMap as JM

data State = State
  { memory  :: Memory
  , ptr     :: Byte
  , jumpMap :: JM.JumpMap
  , program :: Program
  , pc      :: Index
  }

blank :: State
blank =
  State
    { memory  = Mem.blank
    , ptr     = 0
    , jumpMap = JM.empty
    , program = Vec.fromList [JumpAhead, JumpBack]
    , pc      = Index 0
    }

incPtr :: State -> State
incPtr state = state {ptr = ptr state + 1}

decPtr :: State -> State
decPtr state = state {ptr = ptr state - 1}

incPc :: State -> State
incPc state =
  let (Index pc') = pc state
  in state {pc = Index (pc' + 1)}

initState :: Program -> State
initState prog =
  let jm = JM.makeJumpMap prog
  in blank {jumpMap = jm, program = prog}

getDest :: Index -> State -> Index
getDest idx state =
  case Map.lookup idx (jumpMap state) of
    Just dest -> dest
    Nothing ->
      error $
      "unable to find a match for jump instruction at " ++ show idx ++ "!" -- shouldn't happen

example :: Program
example =
  Vec.fromList
    [JumpAhead, JumpAhead, PrintChar, JumpBack, JumpBack, JumpAhead, JumpBack]
