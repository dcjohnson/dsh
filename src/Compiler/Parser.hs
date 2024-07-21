module Compiler.Parser
  ( parse
  , SuccessOrFail(..)
  ) where


import System.IO
import Compiler.ByteCode
import Compiler.Tokenizer

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
    p = Parser
        { tabType = None
        , tabWidth = 0
        , currentTabLevel = 0
        , nextTabLevel = 0
        }
    parseHelper parser [] code = Success code
    parseHelper parser tokens code =
      let
        (t:rest) = tokens
      in
        case t of
          NullWhitespace -> parseHelper parser rest code
          (TabWhitespace n) -> parseHelper (parser { tabType = Tab, tabWidth = n, currentTabLevel = 1 }) rest code
          (SpaceWhitespace n) -> parseHelper (parser { tabType = Space, tabWidth = n, currentTabLevel = 1 }) rest code
          (ExecCommand s) ->
            let
              -- Aggregated args will eventually need to generate Intermediate code so that variables and expressions can be
              -- evaluated in the exec syntax.
              aggregateArgs (arg:restArgs) aggregatedArgs =
                case arg of
                  (StringToken s) -> aggregateArgs restArgs (aggregatedArgs ++ [s])
                  (IntegerToken n) -> aggregateArgs restArgs (aggregatedArgs ++ [show s])
                  (Variable s) -> Fail "Parser can't generate op codes for variables yet"
                  (TabWhitespace _) -> aggregateArgs restArgs aggregatedArgs
                  (SpaceWhitespace _) -> aggregateArgs restArgs aggregatedArgs
                  EndLine -> Success (aggregatedArgs, restArgs)
                  _ -> Fail "Currently unsupported syntax encoutered"
            in
              case (aggregateArgs rest []) of
                (Success (args, restArgs)) -> parseHelper parser restArgs (code ++ [Exec s args])
                (Fail s) -> Fail s
                  
  in
    parseHelper p tokens []
