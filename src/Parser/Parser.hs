module Parser.Parser
  (
    P(..),
    Tester(..),
    test,
    Inst(..)
  ) where

import System.IO
import Control.Monad

-- a is the buffer being parsed
-- e is the error
-- r is the result
data Parser a = Parser a

instance Functor Parser where
  fmap f (Parser a) = Parser (f a)

instance Applicative Parser where
  pure a = Parser a
  (<*>) (Parser f) (Parser a) = Parser (f a)

instance Monad Parser where 
  (>>=) (Parser a) f = f a
  return = pure
  (>>) (Parser a) (Parser b) = Parser b

  
-- pprint :: Parser String e r -> IO ()
-- pprint (Parser a _ _) = putStrLn a

class P a where
  tprint :: a -> IO ()

data Inst a = Inst a
instance (Show a) => P (Inst a) where
  tprint (Inst a) = putStrLn (show a) 

data Tester a = T a

test (T a) = tprint a

