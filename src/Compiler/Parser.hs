module Compiler.Parser
  ( parse
  , SuccessOrFail(..)
  ) where

import Compiler.Code as C
import Compiler.Tokenizer
import Data.Array

data TabType
  = Space
  | Tab
  | None

data SuccessOrFail a b
  = Success a
  | Fail b deriving Show 

data Parser = Parser
  { tabType :: TabType
  , tabWidth :: Int
  , currentTabLevel :: Int
  , nextTabLevel :: Int
  }

parse :: Tokens -> SuccessOrFail Code String
parse tokens =
  let
    parseHelper _ [] code = Success (code ++ [C.Exit])
    parseHelper parser (t:rest) code =
        case t of
          NullWhitespace -> parseHelper parser rest code
          (TabWhitespace n) -> parseHelper (parser { tabType = Tab, tabWidth = n, currentTabLevel = 1 }) rest code
          (SpaceWhitespace n) -> parseHelper (parser { tabType = Space, tabWidth = n, currentTabLevel = 1 }) rest code
          (ExecCommand ec) ->
            let
              -- Aggregated args will eventually need to generate Intermediate code so that variables and expressions can be
              -- evaluated in the exec syntax.
              aggregateArgs [] aggregatedArgs = Success (aggregatedArgs, [])
              aggregateArgs (arg:restArgs) aggregatedArgs =
                case arg of
                  (StringToken st) -> aggregateArgs restArgs (aggregatedArgs ++ [st])
                  (IntegerToken n) -> aggregateArgs restArgs (aggregatedArgs ++ [show n])
                  (Name _) -> Fail "Parser can't generate op codes for variables yet"
                  (TabWhitespace _) -> aggregateArgs restArgs aggregatedArgs
                  (SpaceWhitespace _) -> aggregateArgs restArgs aggregatedArgs
                  EndLine -> Success (aggregatedArgs, restArgs)
                  _ -> Fail "Currently unsupported syntax encoutered"
            in
              case (aggregateArgs rest []) of
                (Success (args, restArgs)) -> parseHelper parser restArgs (code ++ [C.Exec ec args])
                (Fail s) -> Fail s
          _ -> Fail "Currently unsupported tokens encountered"
  in
    case (parseHelper (Parser
                      { tabType = None
                      , tabWidth = 0
                      , currentTabLevel = 0
                      , nextTabLevel = 0
                      }) tokens []) of 
      Success codeList -> Success (listArray (0, length codeList - 1) codeList)
      Fail msg -> Fail msg
