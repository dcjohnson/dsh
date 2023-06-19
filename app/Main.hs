module Main where

import Lib
import Parser.Parser

reeee :: (Show a) => a -> IO ()
reeee a = putStrLn (show a)

main :: IO ()
main = do
  test (T (Inst "abc"))
  reeee "abcd"
