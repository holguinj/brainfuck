module Brainfuck.JumpMap where

import           Brainfuck.Types (Command (..), Index (..), Program)
import qualified Data.Map        as Map
import qualified Data.Vector     as Vec

type JumpMap = Map.Map Index Index

empty :: JumpMap
empty = Map.empty

type NumberedProgram = [(Index, Command)]

data Jump = Ahead | Back

type NumberedJump = (Index, Jump)

onlyJumps :: NumberedProgram -> [NumberedJump]
onlyJumps [] = []
onlyJumps ((idx, JumpAhead):xs) = (idx, Ahead) : onlyJumps xs
onlyJumps ((idx, JumpBack):xs)  = (idx, Back) : onlyJumps xs
onlyJumps (_:xs)                = onlyJumps xs

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

makeJumpMap :: Program -> JumpMap
makeJumpMap prog =
  let numbered = zip indexes (Vec.toList prog)
      jumps = onlyJumps numbered
  in bidirectionalize $ jumpMap' Map.empty [] jumps
  where
    indexes :: [Index]
    indexes = map Index ([0 ..] :: [Int])
