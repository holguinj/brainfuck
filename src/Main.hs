module Main where

import System.Environment (getArgs)
import Brainfuck.Execute (runString)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--help":_) -> showHelp
    ("-h":_) -> showHelp
    ("--file":path:_) -> runFile path
    ("-f":path:_) -> runFile path
    (prog:_) -> runString prog
    _ -> showHelp


runFile :: FilePath -> IO ()
runFile path = do
  contents <- readFile path
  runString contents

showHelp :: IO ()
showHelp =
  putStrLn $
  unlines [ "brainfuck: a minimal interpreter for the brainfuck programming language"
          , ""
          , "  --help, -h            print usage information"
          , "  --file, -f PATH       execute a brainfuck source file"
          , "  \"SOURCECODE\"          execute SOURCECODE directly"]
