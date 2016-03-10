{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.Reader

-- | 48-bit medium access control (MAC) address.
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
  -> IO Frame
receiveOnNIC nic = do
  -- __NOTE__: by reading the promiscuous setting before initiating
  -- the receieve, we cannot change this setting in-flight.
  promis <- readTVarIO (promiscuous nic)
  asyncs <- mapM (async . atomically . portAction promis) . V.toList . ports $ nic
  fmap snd . waitAnyCancel $ asyncs
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

------------------
-- SimpleNodeOp --   
------------------

-- | Data type representing a single-interface single-port node.
data SimpleNode = SimpleNode
  { interface :: {-# UNPACK #-} !NIC
  , handleFrame :: Frame -> IO (Maybe Frame)
  }

newtype SimpleNodeOp a = SimpleNodeOp
  { runSimpleNodeOp :: ReaderT SimpleNode IO a
  } deriving (Functor, Applicative, Monad, MonadReader SimpleNode)

send
  :: LB.ByteString -- ^ Frame payload.
  -> MAC -- ^ Destination
  -> SimpleNodeOp ()
send payload destination = do
  sn <- ask
  SimpleNodeOp . lift . atomically $ sendOnNIC payload destination (interface sn) 0

receive :: SimpleNodeOp Frame
receive = do
  sn <- ask
  frame <- SimpleNodeOp . lift $ 
    receiveOnNIC (interface sn) >>= handleFrame sn
  maybe receive return frame

setPromiscuous
  :: Bool
  -> SimpleNodeOp ()
setPromiscuous b = do
  nic <- fmap interface ask
  SimpleNodeOp . lift . atomically $ writeTVar (promiscuous nic) b
  
----------
-- Main -- 
----------

main
  = undefined
