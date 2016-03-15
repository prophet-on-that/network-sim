-- | The 'Repeater' type is actually a link-layer switch, with zero
-- intelligence employed when forwarding packets.

module NetworkSim.LinkLayer.Repeater
  ( -- * Repeater
    Repeater (..)
  , new
    -- * Repeater programs
  , Op
  , receive
  , repeater
  ) where

import NetworkSim.LinkLayer

import Control.Concurrent.Async.Lifted
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T

-- | A single-interface switch, indiscriminately copying a request
-- on a port to every other port.
data Repeater = Repeater
  { interface :: {-# UNPACK #-} !NIC
  }

new
  :: Int -- ^ Number of ports. Pre: positive.
  -> IO Repeater
new n
  = Repeater <$> newNIC n True

type Op = ReaderT Repeater    

receive
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Op m (PortNum, InFrame)
receive
  = ReaderT $ \(interface -> nic) -> do
      let
        action = do 
          (portNum, frame) <- receiveOnNIC nic
          let
            dest
              = case destination frame of
                  Broadcast ->
                    broadcastAddr
                  MAC a ->
                    a
          if dest == getMAC nic
            then
              return (portNum, frame)
            else do
              let
                indices
                  = filter (/= portNum) [0 .. portCount nic - 1]
                outFrame
                  = frame { destination = dest }
                forward i = do
                  sendOnNIC outFrame nic i
                  logDebugP (getMAC nic) i . T.pack $ "Forwarding frame from " <> show dest <> " to " <> (show . source) frame
                  
              void $ mapConcurrently forward indices
              action
      action  
                      
-- | A program to run atop a 'Repeater' which will discard any
-- messages to the repeater, implicitly forwarding any frame recieved
-- on a port to every other port.
repeater
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Op m ()
repeater
  = forever $ void receive
