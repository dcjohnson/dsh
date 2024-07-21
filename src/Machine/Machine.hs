module Machine.Machine
    ( ptyTest
    ) where

import Data.Map.Lazy
import Compiler.ByteCode

data Machine = Machine
  { stringVarMap :: Map String String
  , intVarMap :: Map Int String
  , instructionPointer :: Int
  }

execute :: Code -> IO ()
execute code = putStrLn "Hi"
