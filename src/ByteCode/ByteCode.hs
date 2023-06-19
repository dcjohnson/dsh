module ByteCode.ByteCode
  ( ByteCode(..)
  ) where

data ByteCode
  = Exec String [String]
  | InvalidParse deriving (Show)

  
