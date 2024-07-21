module Machine.Environment
    ( ptyTest
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

fork :: IO ()
fork = do
  fPid <- SPP.forkProcess (do
      txtFD <- SPI.openFd "/Users/dcjohnson/dev/wackadoo/text.txt" WriteOnly (SPI.defaultFileFlags { append = True }) 
      dupTo txtFD stdOutput 
      SPP.executeFile "echo" True ["reeeeeee"] Nothing
      return ())                              
  SPP.getProcessStatus True False fPid
  return ()
