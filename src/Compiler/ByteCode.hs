module Compiler.ByteCode
  ( IntermediateCode(..),
    Code
  ) where

import Data.Array

-- commands:
-- exec
-- set env var
-- unset env var
-- pushStringLiteral
-- pushIntegerLiteral
data IntermediateCode
  = Exec String [String]
  | InvalidParse deriving (Show)

type Code = Array Int IntermediateCode
