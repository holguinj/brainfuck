module Brainfuck.Optimized.State where

import           Brainfuck.Optimized.Memory (Byte, Memory)
import qualified Brainfuck.Optimized.Memory as Mem
import           Brainfuck.Types            (Index (..), OptimizedCommand (..),
                                             OptimizedProgram, Times (..))
import qualified Data.Map                   as Map
import qualified Data.Vector                as Vec

data State = State
  { memory  :: Memory
  , ptr     :: Byte
  , jumpMap :: JumpMap
  , program :: OptimizedProgram
  , pc      :: Index
  }

blank :: State
blank =
  State Mem.blank 0 Map.empty (Vec.fromList [OJumpAhead, OJumpBack]) (Index 0)

incPtr :: Times -> State -> State
incPtr (Times times) state = state {ptr = ptr state + times}

decPtr :: Times -> State -> State
decPtr (Times times) state = state {ptr = ptr state - times}

incPc :: State -> State
incPc state =
  let (Index pc') = pc state
  in state {pc = Index (pc' + 1)}

initState :: OptimizedProgram -> State
initState prog =
  let jm = makeJumpMap prog
  in blank {jumpMap = jm, program = prog}

type JumpMap = Map.Map Index Index

type NumberedProgram = [(Index, OptimizedCommand)]

data Jump = Ahead | Back deriving (Show)

type NumberedJump = (Index, Jump)

onlyJumps :: NumberedProgram -> [NumberedJump]
onlyJumps []                     = []
onlyJumps ((idx, OJumpAhead):xs) = (idx, Ahead) : onlyJumps xs
onlyJumps ((idx, OJumpBack):xs)  = (idx, Back) : onlyJumps xs
onlyJumps (_:xs)                 = onlyJumps xs

jumpMap' :: JumpMap -> [NumberedJump] -> [NumberedJump] -> JumpMap
jumpMap' acc []                   []                    = acc
jumpMap' acc stack                (j@(_,Ahead):jumps) = jumpMap' acc (j:stack) jumps
jumpMap' acc ((aidx,Ahead):stack) ((bidx,Back):jumps) = jumpMap' (Map.insert aidx bidx acc) stack jumps
jumpMap' _   []                   ((idx,Back):_)        = error $ "no matching '[' for ']' at " ++ show idx ++ "!"
jumpMap' _   ((idx,_):_)          []                    = error $ "no matching ']' for '[' at " ++ show idx ++ "!"
jumpMap' _   _                    _                     = error "pretty sure this is a bad situation, but I don't know how to put it into words."

bidirectionalize :: Ord a => Map.Map a a -> Map.Map a a
bidirectionalize m =
  let kvs = Map.toList m
      vks = map (\(k, v) -> (v, k)) kvs
  in Map.fromList (kvs ++ vks)

makeJumpMap :: OptimizedProgram -> JumpMap
makeJumpMap prog =
  let numbered = zip indexes (Vec.toList prog)
      jumps = onlyJumps numbered
  in bidirectionalize $ jumpMap' Map.empty [] jumps
  where
    indexes :: [Index]
    indexes = map Index ([0 ..] :: [Int])

getDest :: Index -> State -> Index
getDest idx state =
  case Map.lookup idx (jumpMap state) of
    Just dest -> dest
    Nothing ->
      error $
      "unable to find a match for jump instruction at " ++ show idx ++ "!" -- shouldn't happen
