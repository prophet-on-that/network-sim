module Main where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Control.Concurrent.STM
import STMContainers.Map (Map)
import qualified STMContainers.Map as Map

type IntMap = Map Int

data Port = Port
  { bufferRead :: !(TQueue LB.ByteString)
  , bufferWrite :: !(TQueue LB.ByteString)
  }

type MAC = T.Text             

-- | Network interface controller
data NIC = NIC
  { mac :: {-# UNPACK #-} !MAC
  , portCounter :: !(TVar Int)
  , ports :: !(IntMap Port)
  }

newNIC
  :: MAC
  -> STM NIC
newNIC mac'
  = NIC mac' <$> newTVar 0 <*> Map.new

joinNICs
  :: NIC
  -> NIC
  -> STM ()
joinNICs nic nic' = do
  (p, p') <- newConnectedPorts
  addPort p nic
  addPort p' nic'
  where
    newConnectedPorts :: STM (Port, Port)
    newConnectedPorts = do
      q <- newTQueue
      q' <- newTQueue
      return (Port q q', Port q' q)

    addPort
      :: Port
      -> NIC
      -> STM ()
    addPort port nic = do
      n <- readTVar $ portCounter nic
      modifyTVar (portCounter nic) (+ 1)
      Map.insert port n (ports nic)

data Node = Node
  { nics :: !(IntMap NIC)
  }

main
  = undefined
