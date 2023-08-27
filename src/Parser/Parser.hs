module Parser.Parser
  ( execCommand,
    parse
  ) where

import System.IO
import Text.ParserCombinators.ReadP
import Data.Char
import ByteCode.ByteCode

parse :: [String] -> [ByteCode]
parse [] = []
parse (s:_) =
  case readP_to_S execCommand s of
    [((command, n), "")] -> [Exec command n]
    _ -> [InvalidParse]

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

endOfExecTmp =
  choice
  [ eof
  , skipMany1 (char ' ')
  ]

-- "a string"
-- 3 # a number
-- {aVariable}
execArg = do
  choice [ (between (char '"') (char '"') $ do 
             many1 (do
                        c <- get
                        case c of
                          '\\' -> do
                            c' <- get
                            case c' of
                              '\\' -> return c
                              '"' -> return c
                              _ -> pfail
                          _ -> return c)) >>= (\s -> return $ Str s)
         , (munch1 (\c -> isDigit c)) >>= (\d -> return $ Num d)
         , (between (char '{') (char '}') $ do
             munch1 (\c -> isAlphaNum c || c == '_')) >>= (\v -> return $ Var v)
         ]

execCommand = do
  char '@'
  skipSpaces
  command <- manyTill nameParser endOfExecTmp
  skipSpaces
  args <- many (do
                   arg <- execArg
                   skipSpaces
                   return arg)
  skipSpaces
  eof
  return $ (command, args)
