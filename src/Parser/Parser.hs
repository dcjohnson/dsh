module Parser.Parser
  ( execCommand
  ) where

import System.IO
import Text.ParserCombinators.ReadP
import Data.Char

indentationParser indentation = do
  string indentation

nameParser = do
  c <- get
  if isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '/'
    then return c
    else if c == '\\'
         then get
         else pfail
    

endOfExec =
  choice
  [ char '|'
  , char ' '
  ]

-- "a string"
-- 3 # a number
-- {aVariable}
execArg = do
  choice [ between (char '"') (char '"') $ do
             many1 (do
                        c <- get
                        case c of
                          '\\' -> do
                            c' <- get
                            case c' of
                              '\\' -> return c
                              '"' -> return c
                              _ -> pfail
                          _ -> return c)
         , munch1 (\c -> isAlphaNum c)
         , between (char '{') (char '}') $ do
             munch1 (\c -> isAlphaNum c || c == '_')
         ]

execCommand = do
  char '@'
  skipSpaces
  command <- manyTill nameParser endOfExec
  skipSpaces
  args <- many (do
                   arg <- execArg
                   skipSpaces
                   return arg)
  skipSpaces
  eof
  return $ (command, args)
