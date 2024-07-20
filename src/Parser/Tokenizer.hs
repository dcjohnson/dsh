
module Parser.Tokenizer
  ( tokenizeLine,
    tokenize
  ) where

import Text.Read
import Data.Char

data Token =
  IntegerToken Int
  | StringToken String
  | ExecCommand String
  | FunctionCall String
  | BeginVariableName
  | EndVariableName
  | BeginList
  | EndList
  | BeginParen
  | EndParen
  | Background
  | Comma
  | Assign
  | If
  | Else
  | Elsif
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
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
    '('  -> BeginParen
    ')'  -> EndParen
    '['  -> BeginList
    ']'  -> EndList
    '>'  -> GreaterThan
    '<'  -> LessThan
    ','  -> Comma
    '*'  -> Multiplication
    '+'  -> Addition
    '-'  -> Subtraction
    '/'  -> Division
    '%'  -> Modulo
    '|'  -> Pipe
    '\n' -> EndLine
    ' '  -> SpaceWhitespace 1
    '\t' -> TabWhitespace 1
    _    -> Invalid [c]

isCommand :: String -> Bool
isCommand ('@':[]) = False
isCommand ('@':_) = True
isCommand _ = False

isFunctionCall :: String -> Bool
isFunctionCall (c:s) =
  (isAlpha c) && foldr (\s acc -> acc && (c == '_' || (isAlphaNum c))) True s
isFunctionCall [] = False

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
      tokens = (foldl (\acc chunk -> acc ++ tokenizeChunk chunk []) [] . words) line
  in (whitespaceToken:tokens) ++ [EndLine] -- add the newline to ensure that we have delimitation

tokenizeString :: String -> (Token, String)
tokenizeString s =
  let
    stringTokenHelper [] acc = (Invalid acc, "")
    stringTokenHelper (c:rest) acc =
      if c == '"'
      then (StringToken acc, rest)
      else stringTokenHelper rest (acc ++ [c])
  in
    stringTokenHelper s []

tokenizeInteger :: String -> (Token, String)
tokenizeInteger s =
  let
    integerTokenHelper "" acc = (IntegerToken (read acc :: Int), "")
    integerTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if isDigit c
        then integerTokenHelper rest (acc ++ [c])
        else (IntegerToken (read acc :: Int), tokString)
  in integerTokenHelper s []

-- input accumulator output
tokenizeChunk :: String -> Tokens -> Tokens
tokenizeChunk "==" tokens = tokens ++ [EqualityCheck]
tokenizeChunk ":=" tokens = tokens ++ [Assign]
tokenizeChunk "->" tokens = tokens ++ [FileDescriptorRedirection]
tokenizeChunk "" tokens = tokens ++ [NullWhitespace]
tokenizeChunk "if" tokens = tokens ++ [If]
tokenizeChunk "else" tokens = tokens ++ [Else]
tokenizeChunk "elsif" tokens = tokens ++ [Elsif]
tokenizeChunk ">=" tokens = tokens ++ [GreaterThanOrEqual]
tokenizeChunk "<=" tokens = tokens ++ [LessThanOrEqual]
tokenizeChunk ('#':_) tokens = tokens ++ [Comment]
tokenizeChunk "background" tokens = tokens ++ [Background] 
tokenizeChunk s tokens =
  let
    (c:rest) = s
  in
    case c of
      '"' ->
        case tokenizeString rest of 
          (t, "") -> tokens ++ [t]
          (t, newString) -> tokenizeChunk newString (tokens ++ [t])
      '@' -> tokens ++ [ExecCommand rest]
      _ | isDigit c ->
          case tokenizeInteger s of
            (t, "") -> tokens ++ [t]
            (t, newString) -> tokenizeChunk newString (tokens ++ [t])
        | isFunctionCall s -> tokens ++ [FunctionCall s]
        | otherwise -> tokenizeChunk rest (tokens ++ [charToToken c])

isNewline :: Char -> Bool
isNewline c = c == '\n'

isTab :: Char -> Bool
isTab c = c == '\t'

isTrueSpace :: Char -> Bool
isTrueSpace c = c == ' '

isValidSpace :: Char -> Bool
isValidSpace c = (isTab c) || (isTrueSpace c)

createWhitespaceToken :: String -> Token
createWhitespaceToken s
  | isTabs    s = TabWhitespace (length s)
  | isSpaces  s = SpaceWhitespace (length s)
  | otherwise   = Invalid s

extractWhitespaceToken :: String -> Token
extractWhitespaceToken (f_p:rest_p) =
  let
    extract _ [] result = createWhitespaceToken result
    extract c (f:rest) result =
        if c == f
        then extract c rest (f:result)
        else if isValidSpace f
             then MixedWhitespace
             else createWhitespaceToken result
  in if isValidSpace f_p
     then extract f_p rest_p [f_p]
     else NullWhitespace
