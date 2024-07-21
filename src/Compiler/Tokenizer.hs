
module Compiler.Tokenizer
  ( tokenize,
    Token(..),
    Tokens
  ) where

import Data.Char

data Token
  = IntegerToken Int
  | StringToken String
  | Variable String
  | ExecCommand String
  | Exit
  | Return
  | Function
  | Namespace
  | NamespaceDescent
  | Name String
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
  | Invalid String deriving (Show, Eq)

isInvalid :: Token -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False

charToToken :: Char -> Token
charToToken c =
  case c of 
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

isAlphaNumericsChar :: Char -> Bool
isAlphaNumericsChar c = c == '_' || (isAlphaNum c)

type Tokens = [Token]

tokenize :: String -> Tokens
tokenize s =
  let tokenizerChain = (foldr (\line acc ->  (tokenizeLine line) ++ acc) []) .
                       (foldr (\line acc ->
                                  case line of
                                    "" -> acc
                                    _ -> (line:acc)) []) . lines
  in tokenizerChain s

-- We need to preserve leading whitespace as its own token.
tokenizeLine :: String -> Tokens
tokenizeLine line =
  let
    tokens = tokenizeChunks line
  in
    tokens ++ [EndLine] -- add the newline to ensure that we have delimitation

-- Add escape chars at some point
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

tokenizeVariable :: String -> (Token, String)
tokenizeVariable s =
  let
    variableTokenHelper [] acc = (Invalid acc, "")
    variableTokenHelper (c:rest) acc =
      if c == '}'
      then (Variable acc, rest)
      else if isAlphaNumericsChar c
           then variableTokenHelper rest (acc ++ [c])
           else (Invalid acc, "")
  in
    variableTokenHelper s []

tokenizeInteger :: String -> (Token, String)
tokenizeInteger s =
  let
    integerTokenHelper "" acc = (IntegerToken (read acc :: Int), "")
    integerTokenHelper ('-':tokString) "" = integerTokenHelper tokString "-"
    integerTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if isDigit c
        then integerTokenHelper rest (acc ++ [c])
        else (IntegerToken (read acc :: Int), tokString)
  in integerTokenHelper s []

isCommandChar :: Char -> Bool
isCommandChar c = c == '.' || c == '/' || isAlphaNum c

tokenizeCommand :: String -> (Token, String)
tokenizeCommand s =
  let
    integerTokenHelper "" acc = (ExecCommand acc, "")
    integerTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if isCommandChar c
        then integerTokenHelper rest (acc ++ [c])
        else (ExecCommand acc, tokString)
  in integerTokenHelper s []

convertToAlphaNumericsToken :: String -> Token
convertToAlphaNumericsToken "if" = If
convertToAlphaNumericsToken "else" = Else
convertToAlphaNumericsToken "elsif" = Elsif
convertToAlphaNumericsToken "background" = Background
convertToAlphaNumericsToken "fn" = Function
convertToAlphaNumericsToken "ns" = Namespace
convertToAlphaNumericsToken "return" = Return
convertToAlphaNumericsToken "exit" = Exit
convertToAlphaNumericsToken s = Name s

tokenizeAlphaNumerics :: String -> (Token, String)
tokenizeAlphaNumerics s =
  let
    alphaNumericsTokenHelper "" acc = (convertToAlphaNumericsToken acc, "")
    alphaNumericsTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if isAlphaNumericsChar c
        then alphaNumericsTokenHelper rest (acc ++ [c])
        else (convertToAlphaNumericsToken acc, tokString)
  in alphaNumericsTokenHelper s []

tokenizeTab :: Char -> String -> (Token, String)
tokenizeTab tabChar s =
  let
    spaceTabTokenHelper "" acc = (SpaceWhitespace (length acc), "")
    spaceTabTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if c == tabChar
        then spaceTabTokenHelper rest (acc ++ [c])
        else (SpaceWhitespace (length acc), tokString)
  in spaceTabTokenHelper s []

tokenizeSpaceTab :: String -> (Token, String)
tokenizeSpaceTab s = tokenizeTab ' ' s

tokenizeTabChar :: String -> (Token, String)
tokenizeTabChar s = tokenizeTab '\t' s

tokenizeChunks :: String -> Tokens
tokenizeChunks tokenString =
  let
    helper s tokens =
      let
        (token, rest) = tokenizeChunk s
        newTokens = tokens ++ [token]
      in
        if rest == "" || (isInvalid token)
        then newTokens
        else helper rest newTokens
  in
    helper tokenString []

tokenizeChunk :: String -> (Token, String)
tokenizeChunk "" = (NullWhitespace, "")
tokenizeChunk ('#':_) = (Comment, "")
tokenizeChunk s =
  let
    (c:rest) = s
  in
    case c of
      ':' ->
        case rest of
          (':':rest_p) -> (NamespaceDescent, rest_p)
          ('=':rest_p) -> (Assign, rest_p)
          _ -> (Invalid s, "")
      '-' ->
        case rest of
          ('>':rest_p) -> (FileDescriptorRedirection, rest_p)
          (c_p:_) | isDigit c_p -> tokenizeInteger s
          _ -> (Invalid s, "")
      '>' ->
        case rest of
          ('=':rest_p) -> (GreaterThanOrEqual, rest_p)
          _ -> (Invalid s, "")          
      '<' ->
        case rest of
          ('=':rest_p) -> (LessThanOrEqual, rest_p)
          _ -> (Invalid s, "")
      '=' ->
        case rest of
          ('=':rest_p) -> (EqualityCheck, rest_p)
          _ -> (Assign, rest)
      '"' -> tokenizeString rest 
      ' ' -> tokenizeSpaceTab s 
      '\t' -> tokenizeTabChar s 
      '{' -> tokenizeVariable rest 
      '@' -> tokenizeCommand rest 
      _ | isDigit c -> tokenizeInteger s 
        | isAlpha c -> tokenizeAlphaNumerics s 
        | otherwise -> (charToToken c, rest) 
