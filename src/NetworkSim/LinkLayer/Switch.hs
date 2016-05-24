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
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted
import Data.Maybe
import Data.Time
import qualified ListT
import qualified Focus
import Data.Fixed (mod')
import Control.Concurrent (threadDelay)
import Data.Word
import Control.Monad.Catch

-- | A single-interface switch, which identifies hardware addresses
-- with its ports to more efficiently forward frames. The port will be
-- learned as soon as a message is received from a host. Furthermore,
-- when a host disconnects and reconnects, the learned port will be
-- updated as soon as a new message is received.
data Switch = Switch
  { interface :: {-# UNPACK #-} !NIC
  , mapping :: !(Map MAC (PortNum, UTCTime)) -- ^ (portNum, expireTime) pairs.
  , ageingTime :: !(TVar NominalDiffTime) -- ^ Time-to-live of a cache entry.
  }

deviceName
  = "Switch"
    
new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Word16 -- ^ Number of ports. Pre: positive.
  -> Maybe NominalDiffTime -- ^ Optional 'ageingTime', with default 5 seconds.
  -> m Switch
new n ageingTime = do
  nic <- newNIC n True
  mapping <- atomically' Map.new
  announce $ "Creating new Switch with address " <> (T.pack . show . address) nic
  ageingTVar <- liftIO . newTVarIO $ fromMaybe defaultAgeingTime ageingTime 
  return $ Switch nic mapping ageingTVar
  where
    defaultAgeingTime
      = 5

run
  :: (MonadIO m, MonadCatch m, MonadBaseControl IO m, MonadLogger m)
  => Switch
  -> m ()
run switch = do
  withAsync clearExpired . const . forever $ do 
    (portNum, frame) <- atomically' $ receiveOnNIC nic
    let
      broadcast = do
        let
          indices
            = filter (/= portNum) [0 .. portCount nic - 1]
          dest
            = destinationAddr . destination $ frame
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
        
    -- Update mapping with host information.
    now <- liftIO getCurrentTime
    atomically' $ do
      ageingTime' <- readTVar $ ageingTime switch
      let
        expireTime
          = addUTCTime ageingTime' now
      Map.insert (portNum, expireTime) (source frame) (mapping switch)
    
    case destination frame of
      Broadcast -> do 
        broadcast 
      Unicast dest ->
        when (dest /= address nic) $ do 
          port <- atomically' $ Map.lookup dest (mapping switch)
          case port of
            Nothing -> do
              broadcast
            Just (port', _) -> do
              let
                outFrame
                  = frame { destination = dest }
              atomically' $ sendOnNIC outFrame nic port'
              recordWithPort deviceName (address nic) port' . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
  where
    nic
      = interface switch
        
    clearExpired = do
      now <- liftIO getCurrentTime
      deleted <- atomically' $ do 
        keys <- fmap (fmap fst) . ListT.toReverseList . Map.stream $ mapping switch
        fmap catMaybes . forM keys $ \key -> do
          let
            strat Nothing
              = return (False, Focus.Keep)
            strat (Just (_, expireTime))
              = if now < expireTime
                  then
                    return (False, Focus.Keep)
                  else
                    return (True, Focus.Remove)
          removed <- Map.focus strat key (mapping switch)
          if removed
            then
              return (Just key)
            else
              return Nothing

      forM_ deleted $ \mac ->
        record deviceName (address nic) $ "Clearing database entry for " <> (T.pack . show) mac
        
      liftIO $ do 
        now <- getCurrentTime
        let
          remainder
            = utctDayTime now `mod'` freq
        threadDelay . truncate $ remainder * 1000000
      clearExpired
      where
        freq :: DiffTime
        freq
          = 0.00390625 -- 1/256 seconds.
