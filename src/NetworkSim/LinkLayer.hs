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
  , destinationAddr 
  , InFrame
    -- * Hardware Port 
  , PortNum
  , Port ()
  , newPort
  , PortInfo (..)
    -- * Network Interface Controller (NIC)
  , NIC ()
  , newNIC
  , address
  , portInfo
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
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted (fork)

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
  deriving (Eq, Show)

-- | Retrieve underlying MAC address of a 'Destination'.
destinationAddr
  :: Destination
  -> MAC
destinationAddr Broadcast
  = broadcastAddr
destinationAddr (MAC addr)
  = addr

-- | An Ethernet frame with parsed destination field.
type InFrame = Frame Destination 

data Port = Port
  { mate :: !(TVar (Maybe Port))
  , buffer' :: !(TQueue OutFrame)
  }

newPort :: STM Port
newPort
  = Port <$> newTVar Nothing <*> newTQueue

data PortInfo = PortInfo
  { isConnected :: !Bool
  } deriving (Show)

getPortInfo
  :: Port
  -> STM PortInfo
getPortInfo
  = fmap (PortInfo . isJust) . readTVar . mate

-- | Network interface controller (NIC).
data NIC = NIC
  { mac :: {-# UNPACK #-} !MAC
  , ports :: {-# UNPACK #-} !(Vector Port)
  , promiscuity :: !(TVar Bool)
  , buffer :: !(TQueue (PortNum, InFrame)) -- ^ Buffer of messages filtered by ports.
  }

instance Eq NIC where
  nic == nic'
    = mac nic == mac nic'

newNIC
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Int -- ^ Number of ports. Pre: >= 1.
  -> Bool -- ^ Initial promiscuity setting.
  -> m NIC
newNIC n promis = do
  mac <- liftIO freshMAC
  nic <- atomically $ NIC mac <$> V.replicateM n newPort <*> newTVar promis <*> newTQueue
  V.imapM_ (\i p -> void . fork $ portAction nic i p) $ ports nic
  return nic
  where
    portAction nic i p
      = forever $ do
          frame <- atomically $ readTQueue (buffer' p)
          let
            dest
              = destination frame
          if
            | dest == broadcastAddr -> 
                 atomically $ writeTQueue (buffer nic) (i, frame { destination = Broadcast })
            | dest == mac nic ->
                atomically $ writeTQueue (buffer nic) (i, frame { destination = MAC dest })
            | otherwise -> do
                written <- atomically $ do
                  isPromiscuous <- readTVar (promiscuity nic)
                  when isPromiscuous $
                    writeTQueue (buffer nic) (i, frame { destination = MAC dest })
                  return isPromiscuous
                when (not written) $
                  logDebugP (mac nic) i $ "Dropping frame destined for " <> (T.pack . show) dest

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
            return ()
          Just q ->
            writeTQueue (buffer' q) frame

-- | Wait on all ports of a NIC for the next incoming frame. This is a
-- blocking method. Behaviour is affected by the NIC's promiscuity
-- setting (see 'setPromiscuity').
receiveOnNIC
  :: NIC
  -> STM (PortNum, InFrame)
receiveOnNIC 
  = readTQueue . buffer

-- | Toggle promiscuous mode for a 'NIC'. When promiscuous, a NIC will
-- not drop the frames destined for another NIC. This is a useful
-- operating mode for, e.g. switches.
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

address
  :: NIC
  -> MAC
address
  = mac

portInfo
  :: NIC
  -> STM (Vector PortInfo)
portInfo
  = V.mapM getPortInfo . ports

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
      = T.pack $ show mac <> "(" <> show n <> ")"

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
      = T.pack $ show mac <> "(" <> show n <> ")"
