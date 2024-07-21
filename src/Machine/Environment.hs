module Machine.Environment
    ( ptyTest
    , fork
    , forkExec
    ) where

import System.IO
import Data.String
import System.Posix.IO as SPI
import System.Posix.Terminal as SPT
import System.Posix.IO.ByteString as SPIB
import System.Posix.Process as SPP
import Control.Concurrent

ptyTest :: IO ()
ptyTest = do
  (m, s) <- SPT.openPseudoTerminal
  Control.Concurrent.forkIO (do
                                SPIB.fdWrite s (fromString "Hello!")
                                return ())
  str <- SPIB.fdRead m 10
  print str

fork :: Bool -> IO ()
fork block = do
  fPid <- SPP.forkProcess (do
      txtFD <- SPI.openFd "/Users/dcjohnson/dev/dsh/text.txt" WriteOnly (SPI.defaultFileFlags { append = True }) 
      dupTo txtFD stdOutput 
      SPP.executeFile "sleep" True ["10"] Nothing
      return ())

  SPP.getProcessStatus block False fPid
  return ()

forkExec :: String -> [String] -> Bool -> IO ()
forkExec command args block = do
  pid <- SPP.forkProcess (do
                             -- eventually we will not search the path and will define our own path.
                             -- We can, optionally, inherit the path of the parent shell if we want.
                             SPP.executeFile command True args Nothing
                             return ())
  SPP.getProcessStatus block False pid
  return ()
