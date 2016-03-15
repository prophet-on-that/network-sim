{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module NetworkSim.LinkLayer
  ( -- * MAC
    module NetworkSim.LinkLayer.MAC
    -- * Link-layer exception
  , LinkException (..)
    -- * Ethernet Frame
  , Frame (..)
  , Payload ()
  , OutFrame
  , Destination (..)
  , InFrame
    -- * Hardware Port 
  , PortNum
  , Port ()
  , newPort
    -- * Network Interface Controller (NIC)
  , NIC ()
  , newNIC
  , getMAC
  , portCount
  , connectNICs
  , disconnectPort
  , sendOnNIC
  , receiveOnNIC
  , setPromiscuity
    -- * Logging Utilities
  , logInfo'
  , logDebugP
  , logInfoP
  ) where

import NetworkSim.LinkLayer.MAC

import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.STM.Lifted
import Data.Typeable
import Control.Monad.Catch
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad
import Data.Maybe
import Control.Concurrent.Async.Lifted
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

type PortNum = Int

data LinkException
  = PortDisconnected MAC PortNum
  | PortAlreadyConnected MAC PortNum
  | NoFreePort MAC
  | ConnectToSelf MAC 
  deriving (Show, Typeable)

instance Exception LinkException

type Payload = LB.ByteString

-- | A simplified representation of an Ethernet frame, assuming a
-- perfect physical layer.
data Frame a = Frame
  { destination :: !a 
  , source :: {-# UNPACK #-} !MAC 
  , payload :: !Payload
  }

-- | An outbound Ethernet frame.
type OutFrame = Frame MAC

data Destination
  = Broadcast
  | MAC MAC 

-- | An Ethernet frame with parsed destination field.
type InFrame = Frame Destination 

data Port = Port
  { mate :: !(TVar (Maybe Port))
  , buffer :: !(TQueue OutFrame)
  }

newPort :: STM Port
newPort
  = Port <$> newTVar Nothing <*> newTQueue

-- | Network interface controller (NIC).
data NIC = NIC
  { mac :: {-# UNPACK #-} !MAC
  , ports :: {-# UNPACK #-} !(Vector Port)
  , promiscuity :: !(TVar Bool)
  }

instance Eq NIC where
  nic == nic'
    = mac nic == mac nic'

newNIC
  :: MonadIO m
  => Int -- ^ Number of ports. Pre: >= 1.
  -> Bool -- ^ Initial promiscuity setting.
  -> m NIC
newNIC n promis = do
  mac <- liftIO freshMAC
  atomically $ NIC mac <$> V.replicateM n newPort <*> newTVar promis

-- | Connect two NICs, using the first free port available for each.
connectNICs
  :: (MonadIO m, MonadThrow m, MonadLogger m)
  => NIC
  -> NIC
  -> m ()
connectNICs nic nic' = do
  if nic == nic'
    then
      throwM $ ConnectToSelf (mac nic)
    else do
      (portNum, portNum') <- atomically $ do 
        (portNum, p) <- firstFreePort nic
        (portNum', p') <- firstFreePort nic'
        checkDisconnected nic portNum p
        checkDisconnected nic' portNum' p'
        writeTVar (mate p) (Just p')
        writeTVar (mate p') (Just p)
        return (portNum, portNum')
      logInfoN . T.pack $ "Connected " <> show (mac nic) <> "(" <> show portNum <> ") and " <> show (mac nic') <> "(" <> show portNum' <> ")"
  where
    firstFreePort nic = do
      free <- V.filterM hasFreePort . V.indexed $ ports nic
      if V.length free > 0
        then
          return $ V.head free
        else
          throwM $ NoFreePort (mac nic)
      where
        hasFreePort (_, port) 
          = isNothing <$> readTVar (mate port)

    checkDisconnected
      :: NIC
      -> PortNum
      -> Port
      -> STM ()
    checkDisconnected nic n p = do
      q <- readTVar (mate p)
      when (isJust q) $
        throwM $ PortAlreadyConnected (mac nic) n

disconnectPort
  :: (MonadIO m, MonadLogger m)
  => NIC
  -> PortNum
  -> m ()
disconnectPort nic n
  = case ports nic V.!? n of
      Nothing ->
        -- TODO: alert user to index out of bounds error?
        return ()
      Just p -> do
        atomically $ do 
          mate' <- readTVar (mate p)
          case mate' of
            Nothing ->
              throwM $ PortDisconnected (mac nic) n
            Just q -> do
              -- __NOTE__: We do not check if the mate is already
              -- disconnected.
              writeTVar (mate q) Nothing
              writeTVar (mate p) Nothing
        logInfoP (mac nic) n $ "Disconnected port"

sendOnNIC
  :: OutFrame -- ^ The source MAC here is allowed to differ from the NIC's MAC.
  -> NIC
  -> PortNum
  -> STM ()
sendOnNIC frame nic n 
  = case ports nic V.!? n of
      Nothing -> 
        -- TODO: alert user to index out of bounds error?
        return ()
      Just p -> do
        mate' <- readTVar (mate p)
        case mate' of
          Nothing ->
            throwM $ PortDisconnected (mac nic) n
          Just q ->
            writeTQueue (buffer q) frame

-- | Wait on all ports of a NIC for the next incoming frame. This is a
-- blocking method. Behaviour is affected by the NIC's promiscuity
-- setting (see 'setPromiscuity').
receiveOnNIC
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => NIC
  -> m (PortNum, InFrame)
receiveOnNIC nic = do
  -- __NOTE__: by reading the promiscuous setting before initiating
  -- the receieve, we cannot change this setting in-flight.
  promis <- readTVarIO (promiscuity nic)
  asyncs <- V.mapM (\(i, p) -> async $ portAction promis i p) . V.indexed . ports $ nic
  fmap snd . waitAnyCancel . V.toList $ asyncs
  where
    portAction isPromiscuous i p
      = action
      where
        action = do
          frame <- atomically $ readTQueue (buffer p)
          let
            dest
              = destination frame 
          if
            | dest == broadcastAddr -> 
                return (i, frame { destination = Broadcast })
            | dest == mac nic ->
                return (i, frame { destination = MAC dest })
            | otherwise ->
                if isPromiscuous
                  then
                    return (i, frame { destination = MAC dest })
                  else do
                    logDebugP (mac nic) i $ "Dropping frame destined for " <> (T.pack . show) dest
                    action

setPromiscuity
  :: (MonadIO m, MonadLogger m)
  => NIC
  -> Bool
  -> m ()
setPromiscuity nic b = do
  old <- atomically $ swapTVar (promiscuity nic) b
  when (old /= b) $
    if b
      then
        logInfo' (mac nic) $ "Enabling promiscuity mode"
      else
        logInfo' (mac nic) $ "Disabling promiscuity mode"

getMAC
  :: NIC
  -> MAC
getMAC
  = mac

portCount
  :: NIC
  -> Int
portCount
  = V.length . ports

---------------
-- Utilities --
---------------

logInfo'
  :: MonadLogger m
  => MAC
  -> T.Text
  -> m ()
logInfo' mac 
  = logInfoNS sourceStr
  where
    sourceStr
      = T.pack . show $ mac

logInfoP
  :: MonadLogger m
  => MAC
  -> PortNum
  -> T.Text
  -> m ()
logInfoP mac n 
  = logInfoNS sourceStr
  where
    sourceStr
      = T.pack $ show mac <> " (" <> show n <> ")"

logDebugP
  :: MonadLogger m
  => MAC
  -> PortNum
  -> T.Text
  -> m ()
logDebugP mac n 
  = logDebugNS sourceStr
  where
    sourceStr
      = T.pack $ show mac <> " (" <> show n <> ")"
