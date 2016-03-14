{-# LANGUAGE ViewPatterns #-}

module NetworkSim.LinkLayer.SimpleNode
  ( SimpleNode ()
  , new
  , Op
  , send
  , receive
  ) where

import NetworkSim.LinkLayer

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Control

-- | A single-interface, single-report network node.
data SimpleNode = SimpleNode
  { interface :: {-# UNPACK #-} !NIC
  }

new
  :: IO SimpleNode
new
  = SimpleNode <$> newNIC 1 False

type Op = ReaderT SimpleNode

send
  :: (MonadIO m, MonadLogger m)
  => Payload
  -> MAC -- ^ Destination address.
  -> Op m ()
send payload dest
  = ReaderT $ \(interface -> nic) -> do 
      let
        frame
          = Frame dest (getMAC nic) payload
      sendOnNIC frame nic 0
  
receive
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Op m InFrame
receive
  = ReaderT $ \node ->
      snd <$> receiveOnNIC (interface node)
