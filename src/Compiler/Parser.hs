module Compiler.Parser
  ( parse
  ) where

import Compiler.Code as C
import Compiler.Tokenizer as TOK
import Compiler.Errors
import Data.Array
import Data.Map.Lazy as LM

data TabType
  = Space
  | Tab
  | None

data Function = Function
  { name :: String
  , stackSize :: Int
  , parameterCount :: Int
  , callAddress :: Int
  }

incrementStackSize :: Function -> Function
incrementStackSize f =
  let
    size = stackSize f
  in
    f { stackSize = (size + 1) }
    

data Variable
  = StringVar
    { name :: String
    , value :: String
    }
  | IntVar
    { name :: String
    , value :: Int
    }

data Parser = Parser
  { tabType :: TabType
  , tabWidth :: Int
  , currentTabLevel :: Int
  , nextTabLevel :: Int
  , functionTable :: LM.Map String Function
  , variableTable :: LM.Map String Variable
  , opCode :: C.Code
  , tokens :: Tokens
  , currentScope :: String
  }

getToken :: Parser -> Token
getToken p = head $ tokens p

hasToken :: Parser -> Bool
hasToken p = 0 > length (tokens p)

popToken :: Parser -> Parser
popToken p = p { tokens = tail (tokens p) }

functionExists :: Parser -> String -> Bool
functionExists p k = LM.member k (functionTable p)

updateFunction :: Parser -> String -> Function -> Parser
updateFunction p k f = p { functionTable = LM.insert k f (functionTable p) }

incrementFunctionStackSize :: Parser -> SuccessOrFail Parser
incrementFunctionStackSize p =
  let
    key = currentScope p
  in
    case (getFunction p) of
      (Just f) -> Success $ updateFunction p key (incrementStackSize f)
      Nothing -> functionNotFound

getFunction :: Parser -> Maybe Function
getFunction p = LM.lookUP (functionTable p) (currentScope p)

atEndline :: Parser -> Bool
atEndline p = (getToken p) == EndLine

insertVariable :: Parser -> String -> Variable
insertVariable p s v = p { variableTable = LM.insert key v (variableTable p) }

addStringVar :: Parser -> String -> Parser
addStringVar p s =
  let
    key = "::" ++ s
    v = StringVar { name = key, value = s }
  in
    insertVariable p key v

addIntVar :: Parser -> Int -> Parser
addIntVar p i =
  let
    key = "::" ++ (show i :: String)
    v = IntVar { name = key, value = i }
  in
    insertVariable p key v

addOpCode :: Parser -> OpCode -> Parser
addOpCode p oc =
  let
    code = opCode p
  in
    p { opCode = (oc:code) }

pullTokensUntilEndline :: Parser -> SuccessOrFail (Parser, Tokens)
pullTokensUntilEndline p =
  let
    puller parser acc =
      case (tokens parser) of
        (EndLine:rest) -> Success ((p { tokens = rest }), acc)
        (t:rest) -> puller (p { tokens = rest }) (acc ++ [t])
        _ -> Fail "Expected end of line when none was found"
  in
    puller p []

-- THIS IS THE FIRST START OF THE IMPLEMENTATION OF A 2 PASS PARSER

-- parses a series of whitespace delimited expressions
-- a (3 + (2 * a) + 1) "eeee" is a list of expressions.
-- Algebraic expressions must be enclosed with parenthesis
parseExpressionListP1 :: Parser -> SuccessOrFail Parser
parseExpressionListP1 parser =
  let
    helper parser =
      let
        token = getToken parser
      in
        case (isTokenAlgebraic token, atEndline parser) of
          (True, False) -> 
            case (parseExpressionP1 parser) of
              (Success newParser) ->
                -- we don't need to increment the stack since this is a parameter that is being added
                -- to the next function. All values needed to compute the pushed value were accounted
                -- for when the expressions were being evaluated.
                case atEndline newParser of
                  True -> Success newParser
                  False -> helper newParser
              (Fail f) -> Fail f
          (False, True) -> Success parser 
          (False, False) -> invalidTokenInParameterList token
        
  in
    helper parser 
    
    

parseExpressionListP2 :: Parser -> SuccessOrFail Parser
parseExpressionListP2 parser = 

-- algebraic expressions will increase the size of the currently parsed function while being evaluated.
-- this will be represented. However, the emitted code is only the code that computes the value. 
parseExpressionP1 :: Parser -> SuccessOrFail Parser
parseExpressionP1 parser =
  case (getToken parser) of
    (IntegerToken v) ->
      Success (addIntVar (popToken parser) v)
    (StringToken s) ->
      Success (addStringVar (popToken parser) s)
      -- we can have an instruction that is just "push <value or register>"
    _ -> currentlyUnsupportedToken (getToken parser)
    
parseExpressionP2 :: Parser -> SuccessOrFail Parser
parseExpressionP2 parser = 
  case (getToken parser) of
    (IntegerToken v) ->
      Success $ addOpCode (popToken parser) (PushInt v) 
    (StringToken s) ->
      Success $ addOpCode (popToken parser) (PushStr s) 
    _ -> currentlyUnsupportedToken (getToken parser)

parseStatementP1 :: Parser -> SuccessOrFail Parser
parseStatementP1 parser =
  case (tokens p) of
    -- can be assignment or function call
    ((Name s):_) ->
      let
        popedParser = popToken parser
      in
        case getToken popedParser of
          -- if it is an assignment then we must parse the following expression
          Assign -> (parseExpressionP1 . popToken) popedParser
          -- add extra stack space for the var it is ultimately written to. 
          _ -> parseExpressionListP1 popedParser

parseStatementP2 :: Parser -> SuccessOrFail Parser
parseStatementP2 parser =
  case (tokens p) of
    (t:rest) ->
      case t of
        (ExecCommand _) -> 
    

parseFunctionSignatureP1 :: Parser -> SuccessOrFail Parser 
parseFunctionSignatureP1 parser =
  case pullTokensUntilEndline parser of
    (Fail s) -> Fail s
    (Success (newParser, tokens)) ->
      let 
        fnName = Prelude.take 2 tokens
        paramTokens = Prelude.drop 2 tokens
      in
        case fnName of
          [TOK.FunctionToken, Name name] ->
            if functionExists newParser name
            then duplicateFunctionError name
            else 
              let
                paramsValid = Prelude.foldr (\p acc -> (isName p) && acc) True paramTokens
                paramCount = length paramTokens
              in
                if paramsValid
                then Success (updateFunction newParser name (Function { name = name, parameterCount = paramCount }))
                else invalidParameterList name
          _ -> nameSymbolAfterFunction


-- THIS IS THROWAWAY
parse :: Tokens -> SuccessOrFail Code
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
