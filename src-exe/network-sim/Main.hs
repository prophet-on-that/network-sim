module Main where

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

-- | 48-bit media access control (MAC) address.
type MAC = Int64

type PortNum = Int 

data LinkException
  = PortDisconnected MAC PortNum
  | PortAlreadyConnected MAC PortNum
  | NoFreePort MAC
  deriving (Show, Typeable)

instance Exception LinkException

-- | A simplified representation of an Ethernet frame, assuming a
-- perfect physical layer.
data Frame = Frame
  { destination :: {-# UNPACK #-} !MAC 
  , source :: {-# UNPACK #-} !MAC
  , payload :: !LB.ByteString
  } 

data Port = Port
  { mate :: !(TVar (Maybe Port))
  , buffer :: !(TQueue Frame)
  }

newPort :: STM Port
newPort
  = Port <$> newTVar Nothing <*> newTQueue

-- | Network interface controller (NIC).
data NIC = NIC
  { mac :: {-# UNPACK #-} !MAC
  , ports :: {-# UNPACK #-} !(Vector Port)
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

send
  :: LB.ByteString -- ^ Payload.
  -> MAC -- ^ Destination.
  -> NIC
  -> PortNum
  -> STM ()
send payload destination nic n 
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
receive
  :: NIC
  -> IO Frame
receive
  = mapM (async . atomically . readTQueue . buffer) . V.toList . ports >=> fmap snd . waitAnyCancel
    
main
  = undefined
