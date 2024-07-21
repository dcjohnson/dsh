module Compiler.Code
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
  | Exit
  | InvalidParse deriving (Show)

type Code = Array Int IntermediateCode
