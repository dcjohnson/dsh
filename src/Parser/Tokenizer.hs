
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
  | FunctionCall String
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
      tokens = (foldl (\acc chunk -> acc ++ tokenizeChunks chunk) [] . words) line
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
tokenizeChunk "if" = (If, "")
tokenizeChunk "else" = (Else, "")
tokenizeChunk "elsif" = (Elsif, "")
tokenizeChunk ('#':_) = (Comment, "")
tokenizeChunk "background" = (Background, "") 
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
      '{' ->
        case tokenizeVariable rest of 
          (t, "") -> (t, "")
          (t, newString) -> (t, newString)
      '@' ->  (ExecCommand rest, "")
      _ | isDigit c ->
          case tokenizeInteger s of
            (t, "") -> (t, "")
            (t, newString) -> (t, newString) 
        | isFunctionCall s -> (FunctionCall s, "") 
        | otherwise -> (charToToken c, "") 


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
