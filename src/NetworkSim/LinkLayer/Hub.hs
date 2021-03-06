-- | The 'Hub' type is actually a link-layer switch, with zero
-- intelligence employed when forwarding packets. This module is
-- intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer.Hub
  ( -- * Hub
    Hub (interface)
  , new
  , run
  ) where

import NetworkSim.LinkLayer

import Control.Concurrent.Async.Lifted
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T
import Data.Word
import Control.Monad.Catch

-- | A single-interface switch, indiscriminately copying a request
-- on a port to every other port.
data Hub = Hub
  { interface :: {-# UNPACK #-} !NIC
  }

deviceName
  = "Hub"

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Word16 -- ^ Number of ports. Pre: positive.
  -> m Hub
new n = do
  nic <- newNIC n True
  announce $ "Creating new Hub with address " <> (T.pack . show . address) nic
  return $ Hub nic

run
  :: (MonadIO m, MonadCatch m, MonadBaseControl IO m, MonadLogger m)
  => Hub
  -> m ()
run (interface -> nic) = do
  forever $ do 
    (portNum, frame) <- atomically' $ receiveOnNIC nic
    let
      dest
        = destinationAddr $ destination frame
    if dest == address nic
      then
        return () 
      else do
        let
          indices
            = filter (/= portNum) [0 .. portCount nic - 1]
          outFrame
            = frame { destination = dest }
          forward i
            = handle handler $ do
                atomically' $ sendOnNIC outFrame nic i
                recordWithPort deviceName (address nic) i . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
            where
              handler (PortDisconnected _ _)
                = return ()
              handler e
                = throwM e

        void $ mapConcurrently forward indices
