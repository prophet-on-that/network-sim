module NetworkSim.LinkLayer.SimpleNode
  ( -- * SimpleNode 
    SimpleNode ()
  , newSimpleNode 
    -- * SimpleNode programs
  , Op
  ) where

import NetworkSim.LinkLayer

import Control.Concurrent.STM
import qualified Data.Vector as V
import Control.Monad.Reader

-- | A single-interface, single-report network node.
data SimpleNode = SimpleNode
  { interface :: {-# UNPACK #-} !NIC
  }

newSimpleNode
  :: MAC
  -> STM SimpleNode
newSimpleNode mac
  = SimpleNode <$> newNIC
  where
    newNIC
      = NIC mac <$> fmap V.singleton newPort <*> newTVar False

instance Node SimpleNode where
  interfaces
    = V.singleton . interface

  -- __Note__: we do not detect if the user requests an interface or
  -- port number other that 0.
  send payload destination _ _ simpleNode
    = sendOnNIC payload destination (interface simpleNode) 0

  -- __Note__: we do not detect if the user requests an interface
  -- number other that 0.
  receive _ simpleNode
    = receiveOnNIC (interface simpleNode)

type Op = ReaderT SimpleNode IO
