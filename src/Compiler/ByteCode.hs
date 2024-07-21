module Compiler.ByteCode
  ( IntermediateCode(..),
    Code
  ) where

import Data.Array

data IntermediateCode
  = Exec String [String]
  | InvalidParse deriving (Show)

type Code = Array Int IntermediateCode
