module Brainfuck.State where

import           Brainfuck.Memory (Byte, Memory)
import qualified Brainfuck.Memory as Mem
import           Brainfuck.Types  (Command (..), Index (..), Program)
import qualified Data.Map         as Map
import qualified Data.Vector      as Vec

data State = State
  { memory  :: Memory
  , ptr     :: Byte
  , jumpMap :: JumpMap
  , program :: Program
  , pc      :: Index
  }

blank :: State
blank = State Mem.blank 0 Map.empty (Vec.fromList [JumpAhead, JumpBack]) (Index 0)

incPtr :: State -> State
incPtr state = state { ptr = ptr state + 1 }

decPtr :: State -> State
decPtr state = state { ptr = ptr state - 1 }

incPc :: State -> State
incPc state =
  let (Index pc') = pc state
  in state { pc = Index (pc' + 1) }

initState :: Program -> State
initState prog =
  let jm = makeJumpMap prog
  in blank {jumpMap = jm, program = prog}

type JumpMap = Map.Map Index Index

type NumberedProgram = [(Index, Command)]

data Jump = Ahead | Back

type NumberedJump = (Index, Jump)

onlyJumps :: NumberedProgram -> [NumberedJump]
onlyJumps []                   = []
onlyJumps ((idx,JumpAhead):xs) = (idx, Ahead) : onlyJumps xs
onlyJumps ((idx,JumpBack):xs)  = (idx, Back) : onlyJumps xs
onlyJumps (_:xs)               = onlyJumps xs

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
      vks = map (\(k,v) -> (v,k)) kvs
  in Map.fromList (kvs ++ vks)

makeJumpMap :: Program -> JumpMap
makeJumpMap prog =
  let numbered = zip indexes (Vec.toList prog)
      jumps = onlyJumps numbered
  in
    bidirectionalize $ jumpMap' Map.empty [] jumps
  where
    indexes :: [Index]
    indexes = map Index ([0..] :: [Int])

getDest :: Index -> State -> Index
getDest idx state =
  case Map.lookup idx (jumpMap state) of
    Just dest -> dest
    Nothing -> error $ "unable to find a match for jump instruction at " ++ show idx ++ "!" -- shouldn't happen

example :: Program
example = Vec.fromList [JumpAhead, JumpAhead, PrintChar, JumpBack, JumpBack, JumpAhead, JumpBack]
