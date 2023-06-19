module Main where

import Lib
import Parser.Parser
import Text.ParserCombinators.ReadP

printA [] = return ()
printA (n:rest) = do
  putStrLn n
  printA rest

p :: [[String]] -> IO ()
p [] = return ()
p (nl:l) = do 
  printA nl
  p l

main :: IO ()
main =
  case readP_to_S execCommand "@ -\\ a/b\\ \\-\\ -a_\\b.3/c/d.sh 3 4234 \"abc\"" of
    -- [((command, n), x)] -> do
    --   putStrLn command
    --   p n
    --   putStrLn x
    -- [(_, n)] -> putStrLn $ "Couldn't parse command: " ++ n
    -- [] -> putStrLn "Failed Parse"
    e -> print e
