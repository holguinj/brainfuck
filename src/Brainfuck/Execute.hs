module Brainfuck.Execute where

import qualified Brainfuck.Memory as Mem
import qualified Brainfuck.Parse  as Parse
import qualified Brainfuck.State  as St
import qualified Brainfuck.Types  as T
import           Data.Char        (chr, ord)
import qualified Data.Vector      as Vec

onMemory :: (Mem.Memory -> Mem.Memory) -> St.State -> St.State
onMemory f state = state { St.memory = f (St.memory state) }

runCmd :: T.Command -> St.State -> IO St.State
runCmd T.IncPtr = return . St.incPtr
runCmd T.DecPtr = return . St.decPtr
runCmd T.IncVal =
  \state -> do
    let ptr = St.ptr state
    return $ onMemory (Mem.incVal ptr) state
runCmd T.DecVal =
  \state -> do
    let ptr = St.ptr state
    return $ onMemory (Mem.decVal ptr) state
runCmd T.PrintChar =
  \state -> do
    let ptr = St.ptr state
    let val = Mem.deref ptr (St.memory state)
    putChar (chr val)
    return state
runCmd T.ReadChar =
  \state -> do
    char <- getChar
    let val = ord char
    let ptr = St.ptr state
    return $ onMemory (Mem.setVal ptr val) state
runCmd T.JumpAhead =
  \state -> do
    let ptr = St.ptr state
    let val = Mem.deref ptr (St.memory state)
    let dest = St.getDest (St.pc state) state
    if val == 0
      then return $ state {St.pc = dest}
      else return state
runCmd T.JumpBack =
  \state -> do
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

runProgram :: T.Program -> IO ()
runProgram prog = do
  let state = St.initState prog
  _ <- runProgram' state
  return ()

runString :: String -> IO ()
runString = runProgram . Parse.parseProgram
