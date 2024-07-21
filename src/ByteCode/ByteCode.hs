module ByteCode.ByteCode
  ( ByteCode(..),
    Lexeme(..),
    extractString,
    extractStrings,
  ) where

data Lexeme
  = Str String
  | Num String
  | Var String
  | Pipe
  | Command String
  | NewLine deriving (Show)

extractString :: Lexeme -> String
extractString (Str s) = s
extractString (Num s) = s
extractString (Var s) = s
extractString Pipe = "|"
extractString (Command s) = s
extractString NewLine = "\\n"

extractStrings :: [Lexeme] -> [String]
extractStrings lexes = foldr (\l acc -> ((extractString l):acc)) [] lexes

data ByteCode
  = Exec String [String]
  | InvalidParse deriving (Show)

  
