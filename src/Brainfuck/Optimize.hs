module Brainfuck.Optimize (optimize) where

import           Brainfuck.Types
import           Data.List       (group)
import qualified Data.Vector     as Vec

isOptimizable :: Command -> Bool
isOptimizable IncPtr = True
isOptimizable DecPtr = True
isOptimizable IncVal = True
isOptimizable DecVal = True
isOptimizable _      = False

optimizeOne :: Command -> Int -> OptimizedCommand
optimizeOne IncPtr times = OIncPtr (Times times)
optimizeOne DecPtr times = ODecPtr (Times times)
optimizeOne IncVal times = OIncVal (Times times)
optimizeOne DecVal times = ODecVal (Times times)
optimizeOne PrintChar _  = OPrintChar
optimizeOne ReadChar _   = OReadChar
optimizeOne JumpAhead _  = OJumpAhead
optimizeOne JumpBack _   = OJumpBack

optimizeGroup :: [Command] -> [OptimizedCommand]
optimizeGroup [] = []
optimizeGroup grp@(c:_) =
  let times = length grp in
    if isOptimizable c
      then [optimizeOne c times]
      else replicate times (optimizeOne c 1)

groupCommands :: Program -> [[Command]]
groupCommands  = group . Vec.toList

optimize :: Program -> OptimizedProgram
optimize = Vec.fromList . concatMap optimizeGroup . groupCommands
