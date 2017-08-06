module Brainfuck.Parse where

import qualified Brainfuck.Types as T
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vec


parseProgram :: String -> T.Program
parseProgram = Vec.fromList . Maybe.mapMaybe parseChar


parseChar :: Char -> Maybe T.Command
parseChar c =
  case c of
    '>' -> Just T.IncPtr
    '<' -> Just T.DecPtr
    '+' -> Just T.IncVal
    '-' -> Just T.DecVal
    '.' -> Just T.PrintChar
    ',' -> Just T.ReadChar
    '[' -> Just T.JumpAhead
    ']' -> Just T.JumpBack
    _   -> Nothing
