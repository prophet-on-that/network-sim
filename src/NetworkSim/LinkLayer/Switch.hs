-- | An Ethernet switch which learns which ports hosts are connected
-- to to more efficiently forward frames. There is currently no
-- intelligence concerning loops in the network. This module is
-- intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer.Switch
  ( -- * Switch
    Switch (interface)
  , new
  , run
  ) where

import NetworkSim.LinkLayer

import STMContainers.Map (Map)
import qualified STMContainers.Map as Map
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async.Lifted
import qualified Data.Vector as V
import Data.Maybe

-- | A single-interface switch, which identifies hardware addresses
-- with its ports to more efficiently forward frames. The port will be
-- learned as soon as a message is received from a host. Furthermore,
-- when a host disconnects and reconnects, the learned port will be
-- updated as soon as a new message is received.
data Switch = Switch
  { interface :: {-# UNPACK #-} !NIC
  , mapping :: !(Map MAC PortNum)
  }

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Int -- ^ Number of ports. Pre: positive.
  -> m Switch
new n = do
  nic <- newNIC n True
  mapping <- atomically Map.new
  logInfoN $ "Creating new Switch with address " <> (T.pack . show . address) nic
  return $ Switch nic mapping

run
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Switch
  -> m ()
run switch = do
  let
    nic
      = interface switch
  portCount <- V.length <$> atomically (portInfo nic)
  forever $ do 
    (portNum, frame) <- atomically $ receiveOnNIC nic
    let
      broadcast = do
        let
          indices
            = filter (/= portNum) [0 .. portCount - 1]
          dest
            = destinationAddr . destination $ frame
          outFrame
            = frame { destination = dest }
          forward i = do
            portInfo' <- atomically $ do
              sendOnNIC outFrame nic i
              portInfo nic

            when (fromMaybe False . fmap isConnected $ portInfo' V.!? i) $
              logDebugP (address nic) i . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
        void $ mapConcurrently forward indices
        
    -- Update mapping with host information.
    atomically $ Map.insert portNum (source frame) (mapping switch)
    
    case destination frame of
      Broadcast -> do 
        broadcast 
      Unicast dest ->
        when (dest /= address nic) $ do 
          port <- atomically $ Map.lookup dest (mapping switch)
          case port of
            Nothing -> do
              broadcast
            Just port' -> do
              let
                outFrame
                  = frame { destination = dest }
              atomically $ sendOnNIC outFrame nic port'
              logDebugP (address nic) port' . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
