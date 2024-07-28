module Compiler.Tokenizer
  ( tokenize,
    Token(..),
    Tokens,
    isName,
  ) where

import Data.Char

data Token
  = IntegerToken Int
  | StringToken String
  | ExecCommand String
  | Exit
  | Return
  | FunctionToken
  | Namespace
  | NamespaceDescent
  | Name String
  | Filename String
  | BeginList
  | EndList
  | BeginParen
  | EndParen
  | Background
  | Comma
  | Assign
  | Import
  | If
  | Else
  | Elsif
  | TrueToken
  | FalseToken
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

isTokenAlgebraic :: Token -> Bool
isTokenAlgebraic (IntegerToken _) = True
isTokenAlgebraic (StringToken _) = True
isTokenAlgebraic (Name _) = True
isTokenAlgebraic BeginParen = True
isTokenAlgebraic _ = False

isTokenOperator :: Token -> Bool
isTokenOperator BeginParen = True
isTokenOperator EndParen = True
isTokenOperator GreaterThan = True
isTokenOperator LessThan = True
isTokenOperator GreaterThanOrEqual = True
isTokenOperator LessThanOrEqual = True
isTokenOperator EqualityCheck = True
isTokenOperator InequalityCheck = True
isTokenOperator Multiplication = True
isTokenOperator Addition = True
isTokenOperator Subtraction = True
isTokenOperator Division = True
isTokenOperator Modulo = True
isTokenOperator _ = False

operatorPrecidence :: Token -> Maybe Int
isTokenOperator BeginParen = Just 0
isTokenOperator EndParen = Just 0
isTokenOperator GreaterThan = Just 5
isTokenOperator LessThan = Just 5
isTokenOperator GreaterThanOrEqual = Just 5
isTokenOperator LessThanOrEqual = Just 5
isTokenOperator EqualityCheck = Just 4
isTokenOperator InequalityCheck = Just 5
isTokenOperator Multiplication = Just 7
isTokenOperator Addition = Just 6
isTokenOperator Subtraction = Just 6
isTokenOperator Division = Just 7
isTokenOperator Modulo = Just 7
isTokenOperator _ = Nothing


isName :: Token -> Bool
isName (Name _) = True
isName _ = False

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

isFileChar :: Char -> Bool
isFileChar c = (not . isSpace) c

tokenizeFile :: String -> (Token, String)
tokenizeFile s =
  let
    fileTokenHelper "" acc = (Filename acc, "")
    fileTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if isFileChar c
        then fileTokenHelper rest (acc ++ [c])
        else (Filename acc, tokString)
  in fileTokenHelper s []
  
convertToAlphaNumericsToken :: String -> Token
convertToAlphaNumericsToken "if" = If
convertToAlphaNumericsToken "else" = Else
convertToAlphaNumericsToken "elsif" = Elsif
convertToAlphaNumericsToken "true" = TrueToken
convertToAlphaNumericsToken "false" = FalseToken
convertToAlphaNumericsToken "background" = Background
convertToAlphaNumericsToken "fn" = FunctionToken
convertToAlphaNumericsToken "ns" = Namespace
convertToAlphaNumericsToken "return" = Return
convertToAlphaNumericsToken "exit" = Exit
convertToAlphaNumericsToken "import" = Import
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
    genTabToken acc =
      let
        l = length acc
      in
        case tabChar of
          ' ' -> SpaceWhitespace l
          '\t' -> TabWhitespace l
          _ -> Invalid "Invalid tab"
    spaceTabTokenHelper "" acc = (genTabToken acc, "")
    spaceTabTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if c == tabChar
        then spaceTabTokenHelper rest (acc ++ [c])
        else (genTabToken acc, tokString)
  in spaceTabTokenHelper s []

tokenizeSpaceTab :: String -> (Token, String)
tokenizeSpaceTab s = tokenizeTab ' ' s

tokenizeTabChar :: String -> (Token, String)
tokenizeTabChar s = tokenizeTab '\t' s

tokenIsTab :: Token -> Bool
tokenIsTab (SpaceWhitespace _) = True 
tokenIsTab (TabWhitespace _) = True
tokenIsTab _ = False

tokenizeChunks :: String -> Tokens
tokenizeChunks tokenString =
  let
    helper firstToken s tokens =
      let
        (token, rest) = tokenizeChunk s
        newTokens =
          if token == Comment -- don't even add the comment token 
          then tokens
          else if tokenIsTab token
               then if firstToken
                    then tokens ++ [token]
                    else tokens
               else tokens ++ [token]
      in
        if rest == "" || (isInvalid token)
        then newTokens
        else helper False rest newTokens
  in
    helper True tokenString []

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
          _ -> (Invalid s, "")
      '"' -> tokenizeString rest 
      ' ' -> tokenizeSpaceTab s 
      '\t' -> tokenizeTabChar s 
      '@' -> tokenizeCommand rest
      '!' -> tokenizeFile rest
      _ | isDigit c -> tokenizeInteger s 
        | isAlpha c -> tokenizeAlphaNumerics s 
        | otherwise -> (charToToken c, rest) 
