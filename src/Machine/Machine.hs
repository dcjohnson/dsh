module Machine.Machine
    ( execute
    ) where

-- import Data.Map.Lazy
import Data.Array
import Compiler.ByteCode
import Machine.Environment as ENV

-- Need to set env vars
-- Create a separate stack structure for managing function calls
data Machine = Machine
  { instructionPointer :: Int
  }

execute :: Code -> IO ()
execute code = do
  let
    machine = Machine
      { instructionPointer = 0
      }
    executer machine code = do
      let currentInstruction = code ! (instructionPointer machine)
      case currentInstruction of
        (Exec command args) -> ENV.forkExec command args True
        InvalidParse -> putStrLn "Invalid parse"
  executer machine code
      
    
