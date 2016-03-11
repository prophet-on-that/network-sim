{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer
  ( -- * MAC
    MAC
  , printMAC 
  , broadcastAddr
  , LinkException (..)
  , Destination (..)
  , Frame (..)
  , InFrame
  , OutFrame
    -- * Hardware Port 
  , Port (..)
  , newPort
    -- * Network Interface Controller (NIC)
  , NIC (..)
  , connectNICs
  , disconnectPort
  , sendOnNIC
  , receiveOnNIC
    -- * Link-layer Node functionality
  , Node (..)
  , interfacesR
  , sendR
  , receiveR
  ) where

import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.STM
import Data.Typeable
import Control.Monad.Catch
import Data.Int
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad
import Data.Maybe
import Control.Concurrent.Async
import Data.Bits 
import Control.Monad.Reader
import Data.List (intercalate)

-- | 48-bit medium access control (MAC) address.
type MAC = Int64

printMAC :: MAC -> String
printMAC mac
  = intercalate sep . map show $
      [ shiftR mac 40 .&. mask
      , shiftR mac 32 .&. mask
      , shiftR mac 24 .&. mask
      , shiftR mac 16 .&. mask
      , shiftR mac 16 .&. mask
      , mac .&. mask
      ]
  where
    mask
      = 0xffff
    sep
      = ":"

broadcastAddr :: MAC
broadcastAddr
  = 0xffffffffffff

type PortNum = Int
type InterfaceNum = Int 

data LinkException
  = PortDisconnected MAC PortNum
  | PortAlreadyConnected MAC PortNum
  | NoFreePort MAC
  deriving (Show, Typeable)

instance Exception LinkException

-- | A simplified representation of an Ethernet frame, assuming a
-- perfect physical layer.
data Frame a = Frame
  { destination :: !a 
  , source :: {-# UNPACK #-} !MAC 
  , payload :: !LB.ByteString
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
  , promiscuous :: !(TVar Bool)
  }

-- | Connect two NICs, using the first free port available for each.
connectNICs
  :: NIC
  -> NIC
  -> STM ()
connectNICs nic nic' = do
  (portNum, p) <- firstFreePort nic
  (portNum', p') <- firstFreePort nic'
  checkDisconnected nic portNum p
  checkDisconnected nic' portNum' p'
  writeTVar (mate p) (Just p')
  writeTVar (mate p') (Just p)
  where
    firstFreePort nic = do 
      free <- fmap (msum . zipWith (fmap . (,)) [0..] . V.toList) . mapM (readTVar . mate) $ ports nic
      case free of
        Nothing ->
          throwM $ NoFreePort (mac nic)
        Just port ->
          return port

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
  :: NIC
  -> PortNum
  -> STM ()
disconnectPort nic n
  = case ports nic V.!? n of
      Nothing ->
        -- TODO: alert user to index out of bounds error?
        return ()
      Just p -> do
        mate' <- readTVar (mate p)
        case mate' of
          Nothing ->
            throwM $ PortDisconnected (mac nic) n
          Just q -> do
            -- __NOTE__: We do not check if the mate is already
            -- disconnected.
            writeTVar (mate q) Nothing
            writeTVar (mate p) Nothing

sendOnNIC
  :: LB.ByteString -- ^ Payload.
  -> MAC -- ^ Destination.
  -> NIC
  -> PortNum
  -> STM ()
sendOnNIC payload destination nic n 
  = case ports nic V.!? n of
      Nothing -> 
        -- TODO: alert user to index out of bounds error?
        return ()
      Just p -> do 
        let
          frame
            = Frame destination (mac nic) payload
        mate' <- readTVar (mate p)
        case mate' of
          Nothing ->
            throwM $ PortDisconnected (mac nic) n
          Just q ->
            writeTQueue (buffer q) frame

-- | Wait on all ports of a NIC for the next incoming frame. This is a
-- blocking method.
receiveOnNIC
  :: NIC
  -> IO (PortNum, InFrame)
receiveOnNIC nic = do
  -- __NOTE__: by reading the promiscuous setting before initiating
  -- the receieve, we cannot change this setting in-flight.
  promis <- readTVarIO (promiscuous nic)
  asyncs <- mapM (async . atomically . portAction promis) . V.toList . ports $ nic
  let
    indexedAsyncs
      = zipWith (fmap . (,)) [0..] asyncs 
  (portNum, frame) <- fmap snd . waitAnyCancel $ indexedAsyncs
  let
    dest
      = destination frame
    frame'
      = if dest == broadcastAddr
          then
            frame { destination = Broadcast }
          else
            frame { destination = MAC dest }
  return (portNum, frame')
  where
    portAction isPromiscuous p
      = action
      where
        action = do 
          frame <- readTQueue (buffer p)
          if isPromiscuous
            then
              return frame
            else
              if mac nic == destination frame
                then
                  return frame
                else
                  action

class Node n where
  interfaces
    :: n
    -> Vector NIC -- ^ Vector indexed by 'InterfaceNum'.
    
  send
    :: LB.ByteString -- ^ Frame payload.
    -> MAC -- ^ Destination address.
    -> InterfaceNum
    -> PortNum
    -> n
    -> STM ()

  receive
    :: InterfaceNum
    -> n
    -> IO (PortNum, InFrame)

-- | 'ReaderT' version of 'interfaces'.
interfacesR :: (Monad m, Node n) => ReaderT n m (Vector NIC)
interfacesR
  = reader interfaces

-- | 'ReaderT' version of 'send'.
sendR
  :: Node n
  => LB.ByteString
  -> MAC
  -> InterfaceNum
  -> PortNum
  -> ReaderT n STM ()
sendR payload dest i j
  = ReaderT $ send payload dest i j 

-- | 'ReaderT' version of 'receive'.
receiveR
  :: Node n
  => InterfaceNum
  -> ReaderT n IO (PortNum, InFrame)
receiveR i
  = ReaderT $ receive i
