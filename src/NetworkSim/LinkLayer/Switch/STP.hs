-- | This module is intended to be imported qualified.

module NetworkSim.LinkLayer.Switch.STP where

import NetworkSim.LinkLayer

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import STMContainers.Map (Map)
import qualified STMContainers.Map as Map
import Data.Time
import Control.Concurrent.STM.Lifted
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Monoid

data BridgeId = BridgeId
  { priority :: {-# UNPACK #-} !Word16
  , bridgeAddr :: {-# UNPACK #-} !MAC
  } deriving (Eq)

instance Ord BridgeId where
  compare bid bid'
    = case compare (priority bid) (priority bid') of
        GT ->
          LT
        LT ->
          GT
        EQ ->
          case compare (bridgeAddr bid) (bridgeAddr bid') of
            GT ->
              LT
            LT ->
              GT
            EQ ->
              EQ

data BPDU
  = TopologyChange
  | Configuration {-# UNPACK #-} !ConfigurationMessage

data ConfigurationMessage = ConfigurationMessage
  { topologyChange :: !Bool
  , topologyChangeAck :: !Bool
  , rootId :: {-# UNPACK #-} !BridgeId 
  , rootPathCost :: {-# UNPACK #-} !Word32
  , bridgeId :: {-# UNPACK #-} !BridgeId
  , portId :: {-# UNPACK #-} !Word16
  , messageAge :: {-# UNPACK #-} !Word16
  , maxAge :: {-# UNPACK #-} !Word16
  , helloTime :: {-# UNPACK #-} !Word16
  , forwardDelay :: {-# UNPACK #-} !Word16
  }

instance Eq ConfigurationMessage where
  msg == msg'
    = rootId msg == rootId msg' &&
      rootPathCost msg == rootPathCost msg' &&
      bridgeId msg == bridgeId msg' &&
      portId msg == portId msg'

instance Ord ConfigurationMessage where
  compare msg msg'
    = case compare (rootId msg) (rootId msg') of
        LT ->
          GT
        GT ->
          LT
        EQ ->
          case compare (rootPathCost msg) (rootPathCost msg') of
            LT ->
              GT
            GT ->
              LT
            EQ ->
              case compare (bridgeId msg) (bridgeId msg') of
                LT ->
                  GT
                GT ->
                  LT
                EQ ->
                  case compare (portId msg) (portId msg') of
                    LT ->
                      GT
                    GT ->
                      LT
                    EQ ->
                      EQ

data PortStatus
  = Blocked
  | Listening
  | Learning
  | Forwarding

data PortData = PortData
  { status :: !PortStatus
  , configuration :: !(Maybe ConfigurationMessage)
  }

data PortAvailability
  = Disabled
  | Available {-# UNPACK #-} !PortData

-- When broadcasting, attempt to send on ports labelled 'Disabled' in
-- case of out of date. If ever receive 'PortDisconnected' exception,
-- switch port status to 'Disabled' and commence topology change
-- procedure.

data CacheEntry = CacheEntry
  { timestamp :: {-# UNPACK #-} !UTCTime -- ^ Time at which the entry was installed into the cache.
  , portNum :: {-# UNPACK #-} !PortNum
  }

data Switch = Switch 
  { interface :: {-# UNPACK #-} !NIC
  , portAvailability :: {-# UNPACK #-} !(Vector (TVar PortAvailability))
  , mapping :: !(Map MAC CacheEntry) 
  }

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Int -- ^ Number of ports. Pre: positive.
  -> m Switch
new n = do
  nic <- newNIC n True
  announce $ "Creating new STP Switch with address " <> (T.pack . show . address) nic
  portAvailability' <- atomically . V.replicateM n $ newTVar Disabled
  mapping' <- atomically Map.new
  return $ Switch nic portAvailability' mapping'

run
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Switch
  -> m ()
run switch = do 
  atomically initialisePortAvailability
  where
    initialisePortAvailability :: STM ()
    initialisePortAvailability = do
      portInfo' <- portInfo . interface $ switch
      V.forM_ (V.indexed portInfo') $ \(i, info) ->
        writeTVar (portAvailability switch V.! i) $
          if isConnected info
            then
              Available $ PortData Blocked Nothing
            else
              Disabled
