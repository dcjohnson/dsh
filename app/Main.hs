module Main where

import Lib
import Parser.Parser
import Engine.Engine
import Text.ParserCombinators.ReadP
import System.IO
import System.Environment

-- main :: IO ()
-- main =
--   case readP_to_S execCommand "@ -\\ a/b\\ \\-\\ -a_\\b.3/c/d.sh 3 4234 \"abc\"" of
--     -- [((command, n), x)] -> do
--     --   putStrLn command
--     --   p n
--     --   putStrLn x
--     -- [(_, n)] -> putStrLn $ "Couldn't parse command: " ++ n
--     -- [] -> putStrLn "Failed Parse"
--     e -> print e

execute :: String -> IO ()
execute fn = do
  code <- readFile fn
  let commands = (parse . lines) code
      in executeByteCode commands
    

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["-f", fn] -> execute fn
    _ -> putStrLn "Invalid arguments"
