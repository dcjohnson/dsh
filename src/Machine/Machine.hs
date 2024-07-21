module Machine.Machine
    ( execute
    ) where

import Data.Map.Lazy
import Compiler.ByteCode

data Machine = Machine
  { stringVarMap :: Map String String
  , intVarMap :: Map Int String
  , instructionPointer :: Int
  }

execute :: Code -> IO ()
execute _ = putStrLn "Hi"
