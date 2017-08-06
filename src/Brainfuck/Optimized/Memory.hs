module Brainfuck.Optimized.Memory where

import qualified Data.Map as M
import qualified Brainfuck.Types as T

type Byte = Int

type Memory = M.Map Byte Byte

blank :: Memory
blank = M.empty

deref :: Byte -> Memory -> Byte
deref = M.findWithDefault 0

incVal :: T.Times -> Byte -> Memory -> Memory
incVal (T.Times times) ptr memory =
  M.insertWith (+) ptr times memory

decVal :: T.Times -> Byte -> Memory -> Memory
decVal (T.Times times) ptr memory =
  M.insertWith (+) ptr (-times) memory

setVal :: Byte -> Byte -> Memory -> Memory
setVal ptr val memory =
  M.insert ptr val memory
