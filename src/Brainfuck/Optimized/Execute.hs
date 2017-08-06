module Brainfuck.Optimized.Execute where

import qualified Brainfuck.Optimize         as O
import qualified Brainfuck.Optimized.Memory as Mem
import qualified Brainfuck.Optimized.State  as St
import qualified Brainfuck.Parse            as Parse
import qualified Brainfuck.Types            as T
import           Data.Char                  (chr, ord)
import qualified Data.Vector                as Vec

onMemory :: (Mem.Memory -> Mem.Memory) -> St.State -> St.State
onMemory f state = state { St.memory = f (St.memory state) }

runCmd :: T.OptimizedCommand -> St.State -> IO St.State
runCmd (T.OIncPtr times) state = return (St.incPtr times state)
runCmd (T.ODecPtr times) state = return (St.decPtr times state)
runCmd (T.OIncVal times) state = do
  let ptr = St.ptr state
  return $ onMemory (Mem.incVal times ptr) state
runCmd (T.ODecVal times) state = do
  let ptr = St.ptr state
  return $ onMemory (Mem.decVal times ptr) state
runCmd T.OPrintChar state = do
  let ptr = St.ptr state
  let val = Mem.deref ptr (St.memory state)
  putChar (chr val)
  return state
runCmd T.OReadChar state = do
  char <- getChar
  let val = ord char
  let ptr = St.ptr state
  return $ onMemory (Mem.setVal ptr val) state
runCmd T.OJumpAhead state = do
  let ptr = St.ptr state
  let val = Mem.deref ptr (St.memory state)
  let dest = St.getDest (St.pc state) state
  if val == 0
    then return $ state {St.pc = dest}
    else return state
runCmd T.OJumpBack state = do
  let ptr = St.ptr state
  let val = Mem.deref ptr (St.memory state)
  let dest = St.getDest (St.pc state) state
  if val /= 0
    then return $ state {St.pc = dest}
    else return state

runProgram' :: St.State -> IO St.State
runProgram' state = do
  let (T.Index idx) = St.pc state
  case St.program state Vec.!? idx of
    Nothing -> return state
    Just cmd -> do
      newState <- runCmd cmd state
      runProgram' (St.incPc newState)

runProgram :: T.OptimizedProgram -> IO ()
runProgram prog = do
  let state = St.initState prog
  _ <- runProgram' state
  return ()

runString :: String -> IO ()
runString = runProgram . O.optimize . Parse.parseProgram

showOptimized :: String -> String
showOptimized = unwords . map show . Vec.toList . O.optimize . Parse.parseProgram
