module Brainfuck.Types where
import qualified Data.Vector as Vec

data Command
  = IncPtr
  | DecPtr
  | IncVal
  | DecVal
  | PrintChar
  | ReadChar
  | JumpAhead
  | JumpBack
  deriving (Show, Eq)

newtype Index =
  Index Int
  deriving (Eq, Ord, Show)

type Program = Vec.Vector Command

type OptimizedProgram = Vec.Vector OptimizedCommand

newtype Times =
  Times Int
  deriving (Eq, Ord, Show)

data OptimizedCommand
  = OIncPtr Times
  | ODecPtr Times
  | OIncVal Times
  | ODecVal Times
  | OPrintChar
  | OReadChar
  | OJumpAhead
  | OJumpBack

instance Show OptimizedCommand where
  show (OIncPtr (Times times)) = ">(" ++ show times ++ ")"
  show (ODecPtr (Times times)) = "<(" ++ show times ++ ")"
  show (OIncVal (Times times)) = "+(" ++ show times ++ ")"
  show (ODecVal (Times times)) = "-(" ++ show times ++ ")"
  show OPrintChar = "."
  show OReadChar = ","
  show OJumpAhead = "["
  show OJumpBack = "]"
