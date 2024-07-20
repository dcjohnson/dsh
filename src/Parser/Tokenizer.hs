
module Parser.Tokenizer
  ( tokenizeLine,
    tokenize
  ) where

import Text.Read
import Data.Char

data Token =
  IntegerToken Int
  | StringToken String 
  | BeginVariableName
  | EndVariableName
  | BeginList
  | EndList
  | Comma
  | Assign
  | EqualityCheck
  | InequalityCheck
  | Multiplication
  | Addition
  | Subtraction
  | Division
  | Modulo
  | Pipe
  | FileDescriptorRedirection
  | TabWhitespace Int
  | SpaceWhitespace Int
  | NullWhitespace
  | EndLine
  | Comment
  | MixedWhitespace
  | Invalid String deriving Show

charToToken :: Char -> Token
charToToken c =
  case c of 
    '{'  -> BeginVariableName 
    '}'  -> EndVariableName
    '['  -> BeginList
    ']'  -> EndList
    ','  -> Comma
    '*'  -> Multiplication
    '+'  -> Addition
    '-'  -> Subtraction
    '/'  -> Division
    '%'  -> Modulo
    '|'  -> Pipe
    '\n' -> EndLine
    '#'  -> Comment
    ' '  -> SpaceWhitespace 1
    '\t' -> TabWhitespace 1
    _    -> Invalid [c]

stringToToken :: String -> Token
stringToToken (c:[]) = charToToken c
stringToToken "=="   = EqualityCheck
stringToToken ":="   = Assign
stringToToken "->"   = FileDescriptorRedirection
stringToToken ""     = NullWhitespace
stringToToken s
  | isInteger s      = IntegerToken (read s :: Int)
  | isTabs    s      = TabWhitespace (length s)
  | isSpaces  s      = SpaceWhitespace (length s)
  | isString  s      =
    let
      rest = drop 1 s
    in
      StringToken (take (length rest - 1) rest)
  | otherwise        = Invalid s

isString :: String -> Bool
isString s = head s == '"' && last s == '"'

isInteger :: String -> Bool
isInteger s = foldr (\c acc -> acc && (isDigit c)) True s

isCharSequence :: Char -> String -> Bool
isCharSequence seqChar s = foldr (\c acc -> acc && (c == seqChar)) True s

isTabs :: String -> Bool
isTabs s = isCharSequence '\t' s

isSpaces :: String -> Bool
isSpaces s = isCharSequence ' ' s

type Tokens = [Token]

tokenize :: String -> Tokens
tokenize s =
  let tokenizerChain = (foldl (\acc s -> acc ++ (tokenizeLine s)) []) .
                       (foldr (\s acc ->
                                  case s of
                                    "" -> acc
                                    _ -> (s:acc)) []) . lines
  in tokenizerChain s

-- We need to preserve leading whitespace as its own token.
tokenizeLine :: String -> Tokens
tokenizeLine line =
  let whitespaceToken = extractWhitespaceToken line
      tokens = (map (\s -> stringToToken s) . words) line
  in (whitespaceToken:tokens) ++ [EndLine] -- add the newline to ensure that we have delimitation

isNewline :: Char -> Bool
isNewline c = c == '\n'

isTab :: Char -> Bool
isTab c = c == '\t'

isTrueSpace :: Char -> Bool
isTrueSpace c = c == ' '

isValidSpace :: Char -> Bool
isValidSpace c = (isTab c) || (isTrueSpace c)

extractWhitespaceToken :: String -> Token
extractWhitespaceToken (f_p:rest_p) =
  let
    extract _ [] result = stringToToken result
    extract c (f:rest) result =
        if c == f
        then extract c rest (f:result)
        else if isValidSpace f
             then MixedWhitespace
             else stringToToken result
  in if isValidSpace f_p
     then extract f_p rest_p [f_p]
     else NullWhitespace
