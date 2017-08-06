module Main where

import           Brainfuck.Execute           (runString)
import qualified Brainfuck.Optimized.Execute as Optimized
import           System.Environment          (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--help":_)           -> showHelp
    ("-h":_)               -> showHelp
    ("--file":path:_)      -> runFile path
    ("-f":path:_)          -> runFile path
    ("--optimized":path:_) -> runFileOptimized path
    ("-o":path:_)          -> runFileOptimized path
    (prog:_)               -> runString prog
    _                      -> showHelp

runFile :: FilePath -> IO ()
runFile path = do
  contents <- readFile path
  runString contents

runFileOptimized :: FilePath -> IO ()
runFileOptimized path = do
  contents <- readFile path
  Optimized.runString contents

showHelp :: IO ()
showHelp =
  putStrLn $
  unlines
    [ "brainfuck: a minimal interpreter for the brainfuck programming language"
    , ""
    , "  --help, -h               print usage information"
    , "  --file, -f PATH          execute a brainfuck source file"
    , "  --optimized, -o PATH     execute a brainfuck source file with experimental optimizations"
    , "  \"SOURCECODE\"             execute SOURCECODE directly"
    ]
