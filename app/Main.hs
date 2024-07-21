module Main where

import Lib
import Parser.Tokenizer
import Parser.Parser
import ByteCode.ByteCode
import Text.ParserCombinators.ReadP
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
      (print . parse . tokenize) code
    _ -> putStrLn "Invalid arguments"
