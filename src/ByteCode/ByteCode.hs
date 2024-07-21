module ByteCode.ByteCode
  ( IntermediateCode(..),
    Code
  ) where


data IntermediateCode
  = Exec String [String]
  | InvalidParse deriving (Show)

type Code = [IntermediateCode]  
