module Lib
    ( ptyTest
    ) where

import System.IO
import Data.String
import System.Posix.Terminal as SPT
import System.Posix.IO.ByteString as SPIB
import Control.Concurrent
 
ptyTest :: IO ()
ptyTest = do
  (m, s) <- SPT.openPseudoTerminal
  Control.Concurrent.forkIO (do
                                SPIB.fdWrite s (fromString "Hello!")
                                return ())
  (str, _) <- SPIB.fdRead m 10
  System.IO.putStrLn str
