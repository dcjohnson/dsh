module Compiler.Errors
  ( SuccessOrFail(..)
  , duplicateFunctionError
  , invalidParameterList
  , nameSymbolAfterFunction
  ) where

data SuccessOrFail a
  = Success a
  | Fail String deriving Show

duplicateFunctionError name = Fail ("Duplicate function definition of function '" ++ name ++ "'")
invalidParameterList functionName = Fail ("Invalid Parameter List '" ++ functionName ++ "'")
nameSymbolAfterFunction = Fail "Name symbol doesn't follow function indicator"

