module Brainfuck.Memory where

import qualified Data.Map as M

type Byte = Int

type Memory = M.Map Byte Byte

blank :: Memory
blank = M.empty

deref :: Byte -> Memory -> Byte
deref = M.findWithDefault 0

incVal :: Byte -> Memory -> Memory
incVal ptr memory =
  M.insertWith (+) ptr 1 memory

decVal :: Byte -> Memory -> Memory
decVal ptr memory =
  M.insertWith (+) ptr (-1) memory

setVal :: Byte -> Byte -> Memory -> Memory
setVal ptr val memory =
  M.insert ptr val memory
