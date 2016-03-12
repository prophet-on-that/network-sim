-- | The 'Repeater' type is actually a link-layer switch, with zero
-- intelligence employed when forwarding packets.

module NetworkSim.LinkLayer.Repeater
  ( -- * Repeater
    Repeater ()
  , newRepeater
    -- * Repeater programs
  , Op
  , repeater
  ) where

import NetworkSim.LinkLayer

import Control.Concurrent.STM
import qualified Data.Vector as V
import Control.Concurrent.Async
import Control.Monad.Reader

-- | A single-interface switch, indiscriminately copying a request
-- on a port to every other port.
data Repeater = Repeater
  { interface :: {-# UNPACK #-} !NIC
  }

newRepeater
  :: Int -- ^ Number of ports. Pre: positive.
  -> MAC
  -> STM Repeater
newRepeater n mac
  = Repeater <$> newNIC
  where
    newNIC
      = NIC mac <$> V.replicateM n newPort <*> newTVar True

instance Node Repeater where
  interfaces
    = V.singleton . interface

  -- __Note__: we do not detect if the user requests an interface
  -- number other that 0.
  send payload destination _ n repeater
    = sendOnNIC payload destination (interface repeater) n

  -- __Note__: we do not detect if the user requests an interface
  -- number other that 0.
  receive _ repeater = do
    (portNum, frame) <- receiveOnNIC nic
    case destination frame of
      Broadcast -> do 
        forward portNum (payload frame) broadcastAddr 
        receive 0 repeater 
      MAC dest ->
        if dest == mac nic
          then
            return (portNum, frame)
          else do
            forward portNum (payload frame) dest
            receive 0 repeater
    where
      nic
        = interface repeater
          
      forward portNum payload destAddr 
        = void $ mapConcurrently (\i -> atomically $ send payload destAddr 0 i repeater) indices
        where
          indices
            = filter (/= portNum) [0 .. V.length (ports nic) - 1]

type Op = ReaderT Repeater IO

-- | A program to run atop a 'Repeater' which will discard any
-- messages to the repeater, implicitly forwarding any frame recieved
-- on a port to every other port.
repeater :: Op ()
repeater
  = forever $ void (receiveR 0)
