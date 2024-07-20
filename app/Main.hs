module Main where

import Lib
import Parser.Tokenizer
import Parser.Parser
import ByteCode.ByteCode
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

-- execute :: String -> IO ()
-- execute fn = do
--   code <- readFile fn
--   let commands = (parse . lines) code
--       in executeByteCode commands
    

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  -- args <- getArgs
  print (tokenizeLine "\t\t\t\tabcd lkej slekj \"ljks\"")
  print (lines "ablkjd a lakd\n\n  lsja kje la \n jlaks")
  print (tokenize "@ablkjd 134 1234 @a @lakd\n\n  lsja k la \n jlaks")
  print (tokenize " background \"ablkjd\" a @3\"a lakd\n  lsja kje la \n jlaks")
  print (tokenize "\t\t((}}))\t\n\n")
  print (tokenize "   \n   \n\t\n\t\t\n    \n")
  -- case args of
  --   ["-f", fn] -> execute fn
  --   _ -> putStrLn "Invalid arguments"
