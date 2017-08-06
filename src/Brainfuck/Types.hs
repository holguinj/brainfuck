module Brainfuck.Types
  (Command(..), Index(..), Program)
where
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
  deriving (Show)

newtype Index =
  Index Int
  deriving (Eq, Ord, Show)

type Program = Vec.Vector Command
