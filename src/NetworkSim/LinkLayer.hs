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
  , Port ()
  , portCount
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
  , PortConnectHook 
  , setPortConnectHook
    -- * Logging
  , module NetworkSim.LinkLayer.Logging
    -- * Utilities
  , atomically'
  ) where

import NetworkSim.LinkLayer.MAC
import NetworkSim.LinkLayer.Logging

import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.STM
import Data.Typeable
import Control.Monad.Catch
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted (fork)
import Data.Word

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
  } deriving (Show)

-- | An outbound Ethernet frame.
type OutFrame = Frame MAC

data Destination
  = Broadcast
  | Unicast MAC
  deriving (Eq, Show)

-- | Retrieve underlying MAC address of a 'Destination'.
destinationAddr
  :: Destination
  -> MAC
destinationAddr Broadcast
  = broadcastAddr
destinationAddr (Unicast addr)
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

-- | First arg: the local NIC being connected. Second arg: the local port being connected. Third arg: the address of the remote NIC.
type PortConnectHook = NIC -> PortNum -> MAC -> STM ()    

-- | Network interface controller (NIC).
data NIC = NIC
  { mac :: {-# UNPACK #-} !MAC
  , ports :: {-# UNPACK #-} !(Vector Port)
  , promiscuity :: !(TVar Bool)
  , buffer :: !(TQueue (PortNum, InFrame)) -- ^ Buffer of messages filtered by ports.
  , portConnectHook :: !(TVar PortConnectHook) -- ^ Hook guaranteed to be executed atomically after successful 'connectNICs' action. 
  }

instance Eq NIC where
  nic == nic'
    = mac nic == mac nic'

deviceName
  = "NIC"

newNIC
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Word16 -- ^ Number of ports. Pre: >= 1.
  -> Bool -- ^ Initial promiscuity setting.
  -> m NIC
newNIC n promis = do
  mac <- liftIO freshMAC
  nic <- atomically' $ NIC mac <$> V.replicateM (fromIntegral n) newPort <*> newTVar promis <*> newTQueue <*> newTVar defaultHook
  V.imapM_ (\(fromIntegral -> i) p -> void . fork $ portAction nic i p) $ ports nic
  return nic
  where
    portAction nic i p
      = forever $ do
          frame <- atomically' $ readTQueue (buffer' p)
          let
            dest
              = destination frame
          if
            | dest == broadcastAddr -> 
                 atomically' $ writeTQueue (buffer nic) (i, frame { destination = Broadcast })
            | dest == mac nic ->
                atomically' $ writeTQueue (buffer nic) (i, frame { destination = Unicast dest })
            | otherwise -> do
                written <- atomically' $ do
                  isPromiscuous <- readTVar (promiscuity nic)
                  when isPromiscuous $
                    writeTQueue (buffer nic) (i, frame { destination = Unicast dest })
                  return isPromiscuous
                when (not written) $
                  recordWithPort deviceName (mac nic) i $ "Dropping frame destined for " <> (T.pack . show) dest

    defaultHook _ _ _
      = return ()

-- | Connect two NICs, using the first free port available for
-- each. Returns these ports.
connectNICs
  :: (MonadIO m, MonadThrow m, MonadLogger m)
  => NIC
  -> NIC
  -> m (PortNum, PortNum)
connectNICs nic nic' = do
  if nic == nic'
    then
      throwM $ ConnectToSelf (mac nic)
    else do
      ports@(portNum, portNum') <- atomically' $ do 
        (fromIntegral -> portNum, p) <- firstFreePort nic
        (fromIntegral -> portNum', p') <- firstFreePort nic'
        checkDisconnected nic portNum p
        checkDisconnected nic' portNum' p'
        writeTVar (mate p) (Just p')
        writeTVar (mate p') (Just p)
        readTVar (portConnectHook nic) >>= \f -> f nic portNum (address nic')
        readTVar (portConnectHook nic') >>= \f -> f nic' portNum' (address nic)
        return (portNum, portNum')
      announce . T.pack $ "Connected " <> show (mac nic) <> "(" <> show portNum <> ") and " <> show (mac nic') <> "(" <> show portNum' <> ")"
      return ports
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

-- | Will throw a 'PortDisconnected' exception if the port requested port
-- is already disconnected.
disconnectPort
  :: (MonadIO m, MonadLogger m)
  => NIC
  -> PortNum
  -> m ()
disconnectPort nic n
  = case ports nic V.!? (fromIntegral n) of
      Nothing ->
        -- TODO: alert user to index out of bounds error?
        return ()
      Just p -> do
        atomically' $ do 
          mate' <- readTVar (mate p)
          case mate' of
            Nothing ->
              throwM $ PortDisconnected (mac nic) n
            Just q -> do
              -- __NOTE__: We do not check if the mate is already
              -- disconnected.
              writeTVar (mate q) Nothing
              writeTVar (mate p) Nothing
        recordWithPort deviceName (mac nic) n $ "Disconnected port"

-- | Will throw a 'PortDisconnected' exception if you try to send on a
-- disconnected port.
sendOnNIC
  :: OutFrame -- ^ The source MAC here is allowed to differ from the NIC's MAC.
  -> NIC
  -> PortNum
  -> STM ()
sendOnNIC frame nic n 
  = case ports nic V.!? (fromIntegral n) of
      Nothing ->
        -- TODO: alert user to index out of bounds error?
        return ()
      Just p -> do
        mate' <- readTVar (mate p)
        case mate' of
          Nothing ->
            throwM $ PortDisconnected (mac nic) n
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
  old <- atomically' $ swapTVar (promiscuity nic) b
  when (old /= b) $
    if b
      then
        record deviceName (mac nic) $ "Enabling promiscuity mode"
      else
        record deviceName (mac nic) $ "Disabling promiscuity mode"

-- | Set a hook guaranteed to be run atomically after successful
-- execution of 'connectNICs'.
setPortConnectHook
  :: PortConnectHook
  -> NIC
  -> STM ()
setPortConnectHook h nic
  = writeTVar (portConnectHook nic) h
  
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

-- | @ atomically' = liftIO . atomically @
atomically'
  :: MonadIO m
  => STM a
  -> m a 
atomically'
  = liftIO . atomically

portCount
  :: NIC
  -> Word16
portCount
  = fromIntegral . V.length . ports
