module Machine.Machine
    ( execute
    ) where

-- import Data.Map.Lazy
import Data.Array
import Compiler.Code
import Machine.Environment as ENV

-- Need to set env vars
-- Create a separate stack structure for managing function calls
data Machine = Machine
  { instructionPointer :: Int
  }

fetchInstruction :: Machine -> Code -> (OpCode, Machine)
fetchInstruction machine code =
  let
    currentIP = instructionPointer machine
    currentInstruction = code ! currentIP
    nextMachine = machine { instructionPointer = currentIP + 1 }
  in
    (currentInstruction, nextMachine)

execute :: Code -> IO ()
execute code = do
  let
    machine = Machine
      { instructionPointer = 0
      }
    executer machine code = do
      let (currentInstruction, nextMachine) = fetchInstruction machine code
      case currentInstruction of
        (Exit) -> return ()
        (Exec command args) -> do
          ENV.forkExec command args True
          executer nextMachine code
        InvalidParse -> putStrLn "Invalid parse"
  executer machine code
      
    
