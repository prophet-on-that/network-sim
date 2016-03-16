-- | This module is intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LinkLayer.SimpleNode
  ( SimpleNode (interface)
  , new
    -- * SimpleNode API
  , Op
  , runOp
  , send
  , receive
  , atomically
  ) where

import NetworkSim.LinkLayer

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Concurrent.STM.Lifted (STM)
import qualified Control.Concurrent.STM.Lifted as STM
import Data.Monoid
import qualified Data.Text as T

-- | A single-interface, single-port network node.
data SimpleNode = SimpleNode
  { interface :: {-# UNPACK #-} !NIC
  }

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => m SimpleNode
new = do
  nic <- newNIC 1 False
  logInfoN $ "Creating new SimpleNode with address " <> (T.pack . show . address) nic
  return $ SimpleNode nic

newtype Op m a = Op (ReaderT SimpleNode m a)
  deriving (Functor, Applicative, Monad, MonadLogger)

runOp
  :: SimpleNode
  -> Op m a
  -> m a
runOp simpleNode (Op action)
  = runReaderT action simpleNode

-- | This method will automatically set the source address of the
-- frame to that of the underlying 'SimpleNode'.
send
  :: (MonadIO m, MonadLogger m)
  => Payload
  -> MAC -- ^ Destination address.
  -> Op m ()
send payload dest
  = Op . ReaderT $ \(interface -> nic) -> do 
      let
        frame
          = Frame dest (address nic) payload
      STM.atomically $ sendOnNIC frame nic 0
      logInfo' (address nic) $ "Sending frame to " <> (T.pack . show) dest
      
receive
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Op m InFrame
receive
  = Op . ReaderT $ \node -> do 
      frame <- STM.atomically $ snd <$> receiveOnNIC (interface node)
      logInfo' (address . interface $ node) $ "Received frame from " <> (T.pack . show . source) frame
      return frame

-- | Run STM actions in 'Op' programs.
atomically
  :: MonadIO m
  => STM a
  -> Op m a
atomically
  = Op . STM.atomically 
