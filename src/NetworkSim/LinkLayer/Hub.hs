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
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T
import Control.Concurrent.STM.Lifted
import qualified Data.Vector as V
import Data.Maybe

-- | A single-interface switch, indiscriminately copying a request
-- on a port to every other port.
data Hub = Hub
  { interface :: {-# UNPACK #-} !NIC
  }

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Int -- ^ Number of ports. Pre: positive.
  -> m Hub
new n = do
  nic <- newNIC n True
  logInfoN $ "Creating new Hub with address " <> (T.pack . show . address) nic
  return $ Hub nic

run
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Hub
  -> m ()
run (interface -> nic) = do
  portCount <- V.length <$> atomically (portInfo nic)
  forever $ do 
    (portNum, frame) <- atomically $ receiveOnNIC nic
    let
      dest
        = destinationAddr $ destination frame
    if dest == address nic
      then
        return () 
      else do
        let
          indices
            = filter (/= portNum) [0 .. portCount - 1]
          outFrame
            = frame { destination = dest }
          forward i = do
            portInfo' <- atomically $ do
              sendOnNIC outFrame nic i
              portInfo nic
            when (fromMaybe False . fmap isConnected $ portInfo' V.!? i) $
              logDebugP (address nic) i . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
        void $ mapConcurrently forward indices
