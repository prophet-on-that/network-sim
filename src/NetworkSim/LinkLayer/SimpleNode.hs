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

-- | A single-interface, single-report network node.
data SimpleNode = SimpleNode
  { interface :: {-# UNPACK #-} !NIC
  }

new
  :: IO SimpleNode
new
  = SimpleNode <$> newNIC 1 False

newtype Op m a = Op (ReaderT SimpleNode m a)
  deriving (Functor, Applicative, Monad, MonadLogger)

runOp
  :: SimpleNode
  -> Op m a
  -> m a
runOp simpleNode (Op action)
  = runReaderT action simpleNode

send
  :: (MonadIO m, MonadLogger m)
  => Payload
  -> MAC -- ^ Destination address.
  -> Op m ()
send payload dest
  = Op . ReaderT $ \(interface -> nic) -> do 
      let
        frame
          = Frame dest (getMAC nic) payload
      sendOnNIC frame nic 0
  
receive
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Op m InFrame
receive
  = Op . ReaderT $ \node ->
      snd <$> receiveOnNIC (interface node)

-- | Run STM actions in 'Op' programs.
atomically
  :: MonadIO m
  => STM a
  -> Op m a
atomically
  = Op . STM.atomically 
