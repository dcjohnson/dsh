module ByteCode.ByteCode
  ( ByteCode(..),	
		Lexeme(..)
  ) where

data Lexeme
  = Str String
  | Num String
  | Var String deriving (Show)

data ByteCode
  = Exec String [Lexeme]
  | InvalidParse deriving (Show)

  
