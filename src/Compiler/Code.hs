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

-- Global Arg is just a key into a map.
-- OffsetArg is an index from the stack pointer. 
data ExecArg = GlobalArg String | OffsetArg Int 

data OpCode
  = Exec String [String]
  | PushStr String
  | PushInt Int
  | PushReg String
  | Exit
  | InvalidParse deriving (Show)

type Code = Array Int OpCode
