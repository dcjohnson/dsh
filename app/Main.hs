import Compiler.Tokenizer
import Compiler.Parser
import Machine.Machine
import System.IO
import System.Environment    

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["-t", fn] -> do
      code <- readFile fn
      (print . tokenize) code
    ["-f", fn] -> do
      code <- readFile fn
      case (parse . tokenize) code of
        (Success compCode) -> execute compCode
        (Fail s) -> putStrLn s
    _ -> putStrLn "Invalid arguments"

