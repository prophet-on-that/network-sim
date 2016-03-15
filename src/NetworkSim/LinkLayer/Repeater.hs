-- | The 'Repeater' type is actually a link-layer switch, with zero
-- intelligence employed when forwarding packets. This module is
-- intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer.Repeater
  ( -- * Repeater
    Repeater (interface)
  , new
    -- * Repeater API
  , Op
  , runOp
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
import Control.Concurrent.STM.Lifted

-- | A single-interface switch, indiscriminately copying a request
-- on a port to every other port.
data Repeater = Repeater
  { interface :: {-# UNPACK #-} !NIC
  }

new
  :: (MonadIO m, MonadLogger m)
  => Int -- ^ Number of ports. Pre: positive.
  -> m Repeater
new n = do
  nic <- newNIC n True
  logInfoN $ "Creating new Repeater with address " <> (T.pack . show . getMAC) nic
  return $ Repeater nic

newtype Op m a = Op (ReaderT Repeater m a)
  deriving (Functor, Applicative, Monad, MonadLogger)

runOp
  :: Repeater
  -> Op m a
  -> m a
runOp r (Op action)
  = runReaderT action r

receive
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Op m (PortNum, InFrame)
receive
  = Op . ReaderT $ \(interface -> nic) -> do
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
                  atomically $ sendOnNIC outFrame nic i
                  logDebugP (getMAC nic) i . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
                  
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
