
module Parser.Tokenizer
  ( tokenizeLine,
    tokenize
  ) where

import Text.Read
import Data.Char

data Token
  = IntegerToken Int
  | StringToken String
  | Variable String
  | ExecCommand String
  | Function
  | FunctionName String
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
    -- '{'  -> BeginVariableName 
    -- '}'  -> EndVariableName
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
  let tokenizerChain = (foldl (\acc s -> acc ++ (tokenizeLine s)) []) .
                       (foldr (\s acc ->
                                  case s of
                                    "" -> acc
                                    _ -> (s:acc)) []) . lines
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
      else variableTokenHelper rest (acc ++ [c])
  in
    variableTokenHelper s []

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

tokenizeCommand :: String -> (Token, String)
tokenizeCommand s =
  let
    integerTokenHelper "" acc = (ExecCommand acc, "")
    integerTokenHelper tokString acc =
      let
        (c:rest) = tokString
      in
        if isSpace c
        then (ExecCommand acc, tokString)
        else integerTokenHelper rest (acc ++ [c])
  in integerTokenHelper s []

convertToAlphaNumericsToken :: String -> Token
convertToAlphaNumericsToken "if" = If
convertToAlphaNumericsToken "else" = Else
convertToAlphaNumericsToken "elsif" = Elsif
convertToAlphaNumericsToken "background" = Background
convertToAlphaNumericsToken "fn" = Function
convertToAlphaNumericsToken s = FunctionName s

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
tokenizeChunk s  =
  let
    (c:rest) = s
  in
    case c of
      ':' ->
        case rest of
          ('=':rest_p) -> (Assign, rest_p)
          _ -> (Invalid s, "")
      '-' ->
        case rest of
          ('>':rest_p) -> (FileDescriptorRedirection, rest_p)
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
      '"' ->
        case tokenizeString rest of 
          (t, "") -> (t, "")
          (t, newString) -> (t, newString)
      ' ' ->
        case tokenizeSpaceTab s of 
          (t, "") -> (t, "")
          (t, newString) -> (t, newString)
      '\t' ->
        case tokenizeTabChar s of 
          (t, "") -> (t, "")
          (t, newString) -> (t, newString)
      '{' ->
        case tokenizeVariable rest of 
          (t, "") -> (t, "")
          (t, newString) -> (t, newString)
      '@' ->  
        case tokenizeCommand rest of 
          (t, "") -> (t, "")
          (t, newString) -> (t, newString)
      _ | isDigit c ->
          case tokenizeInteger s of
            (t, "") -> (t, "")
            (t, newString) -> (t, newString) 
        | isAlpha c ->
            case tokenizeAlphaNumerics s of 
              (t, "") -> (t, "")
              (t, newString) -> (t, newString)
        | otherwise -> (charToToken c, "") 
