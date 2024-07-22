module Compiler.Code
  ( OpCode(..),
    Code
  ) where

import Data.Array

-- commands:
-- exec
-- set env var
-- unset env var
-- pushStringLiteral
-- pushIntegerLiteral
data OpCode
  = Exec String [String]
  | Exit
  | InvalidParse deriving (Show)

type Code = Array Int OpCode
