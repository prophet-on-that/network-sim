-- | This module is intended to be imported qualified.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module NetworkSim.LinkLayer.Switch.STP where

import NetworkSim.LinkLayer

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import STMContainers.Map (Map)
import qualified STMContainers.Map as Map
import Data.Time
import Control.Concurrent.STM
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Monoid
import Control.Monad
import Control.Concurrent.Async.Lifted
import GHC.Generics
import Data.Binary
import Data.Binary.Put
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Data.Ord

deviceName
  = "STP switch"

type Priority = Word16

defaultPriority :: Priority
defaultPriority
  = 80

data SwitchId = SwitchId
  { priority :: {-# UNPACK #-} !Priority
  , switchAddr :: {-# UNPACK #-} !MAC
  } deriving (Eq, Show, Generic, Binary)

instance Ord SwitchId where
  compare sid sid'
    = case compare (priority sid) (priority sid') of
        GT ->
          LT
        LT ->
          GT
        EQ ->
          case compare (switchAddr sid) (switchAddr sid') of
            GT ->
              LT
            LT ->
              GT
            EQ ->
              EQ

data BPDU
  = TopologyChange
  | Configuration {-# UNPACK #-} !ConfigurationMessage
  deriving (Show)

instance Binary BPDU where
  put TopologyChange = do 
    replicateM_ 3 $ putWord8 0
    putWord8 128

  put (Configuration msg) = do
    replicateM_ 4 $ putWord8 0
    putLazyByteString $ encode msg

  get = do
    replicateM 3 getWord8 >>= guard . all (== 0)
    msum
      [ do
          getWord8 >>= guard . (== 0)
          Configuration <$> get

      , do
          getWord8 >>= guard . (== 128)
          return TopologyChange
      ]

data ConfigurationMessage = ConfigurationMessage
  { topologyChange :: !Bool
  , topologyChangeAck :: !Bool
  , rootId :: {-# UNPACK #-} !SwitchId 
  , rootPathCost :: {-# UNPACK #-} !Word32
  , switchId :: {-# UNPACK #-} !SwitchId
  , portId :: {-# UNPACK #-} !PortNum
  , messageAge :: {-# UNPACK #-} !Word16
  , maxAge :: {-# UNPACK #-} !Word16
  , helloTime :: {-# UNPACK #-} !Word16
  , forwardDelay :: {-# UNPACK #-} !Word16
  } deriving (Show, Generic, Binary)

instance Eq ConfigurationMessage where
  msg == msg'
    = rootId msg == rootId msg' &&
      rootPathCost msg == rootPathCost msg' &&
      switchId msg == switchId msg' &&
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
              case compare (switchId msg) (switchId msg') of
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
  deriving (Eq)

data PortData = PortData
  { status :: !PortStatus
  , configuration :: !(Maybe ConfigurationMessage)
  }

newPortData :: PortData
newPortData
  = PortData Blocked Nothing

data PortAvailability
  = Disabled
  | Available {-# UNPACK #-} !PortData

data SwitchStatus
  = RootSwitch
  | NonRootSwitch {-# UNPACK #-} !NonRootSwitch

data NonRootSwitch = NonRootSwitch'
  { rootPort :: {-# UNPACK #-} !PortNum
  , rootPortId :: {-# UNPACK #-} !SwitchId
  , designatedPorts :: !(Set PortNum)
  , cost :: {-# UNPACK #-} !Word32
  }

-- When broadcasting, attempt to send on ports labelled 'Disabled' in
-- case of out of date. If ever receive 'PortDisconnected' exception,
-- switch port status to 'Disabled' and commence topology change
-- procedure.

data Notification
  = NewMessage {-# UNPACK #-} !PortNum !BPDU

data CacheEntry = CacheEntry
  { timestamp :: {-# UNPACK #-} !UTCTime -- ^ Time at which the entry was installed into the cache.
  , portNum :: {-# UNPACK #-} !PortNum
  }

data Switch = Switch 
  { interface :: {-# UNPACK #-} !NIC
  , portAvailability :: {-# UNPACK #-} !(Vector (TVar PortAvailability))
  , cache :: !(Map MAC CacheEntry)
  , notificationQueue :: !(TQueue Notification)
  , iden :: {-# UNPACK #-} !SwitchId -- ^ TODO: enable dynamic updating of priority.
  , switchStatus :: !(TVar SwitchStatus)
  }

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Word16 -- ^ Number of ports. Pre: positive.
  -> Priority -- ^ Switch priority. See also 'defaultPriority'.
  -> m Switch
new n prio = do
  nic <- newNIC n True
  announce $ "Creating new STP Switch with address " <> (T.pack . show . address) nic
  let
    switchId
      = SwitchId prio $ address nic
  atomically' $ Switch nic
    <$> (V.replicateM (fromIntegral n) $ newTVar Disabled)
    <*> Map.new
    <*> newTQueue
    <*> return switchId
    <*> newTVar RootSwitch

-- | The status of the 'Switch' is re-initialised with each 'run'.
run
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => Switch
  -> m ()
run (Switch nic portAvailability' cache' notificationQueue' iden' switchStatus') = do 
  atomically' initialise
  withAsync stpThread . const . forever $ do
    (sourcePort, frame) <- atomically' $ receiveOnNIC nic
    now <- liftIO getCurrentTime
    atomically' $ updateCache (source frame) sourcePort now
    case destination frame of
      Broadcast ->
        broadcast sourcePort frame
      Unicast dest ->
        if dest == stpAddr
          then
            case decodeOrFail (payload frame) of
              Left (_, _, err) ->
                recordWithPort deviceName (address nic) sourcePort . T.pack $ "Error when deserialising BPDU: " <> err
              Right (_, _, bpdu) ->
                atomically' . writeTQueue notificationQueue' $ NewMessage sourcePort bpdu
          else do
            when (dest /= address nic) $ do 
              port <- atomically' $ Map.lookup dest cache'
              case port of
                Nothing -> do
                  broadcast sourcePort frame
                Just (portNum -> port') -> do
                  let
                    outFrame
                      = frame { destination = dest }
                  atomically' $ sendOnNIC outFrame nic port'
                  recordWithPort deviceName (address nic) port' . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
  where
    initialise :: STM ()
    initialise = do
      initialisePortAvailability
      writeTVar switchStatus' RootSwitch
      where
        initialisePortAvailability = do
          portInfo' <- portInfo nic
          V.forM_ (V.indexed portInfo') $ \(i, info) ->
            writeTVar (portAvailability' V.! i) $
              if isConnected info
                then
                  Available newPortData
                else
                  Disabled
              
    -- Forward broadcast frame on ports in 'Forwarding' state.
    broadcast originPort frame
      = void $ mapConcurrently forward indices
      where
        indices
          = filter (/= originPort) [0 .. portCount nic - 1]
        dest
          = destinationAddr . destination $ frame
        outFrame
          = frame { destination = dest }

        -- TODO: catch exceptions when sending to transition to
        -- disabled state, or poll portInfo vector. Exception catching
        -- preferable allows avoiding logging.
        forward i = do
          sent <- atomically' $ do
            av <- readTVar $ portAvailability' V.! (fromIntegral i)
            case av of
              Available (status -> Forwarding) -> do
                sendOnNIC outFrame nic i
                return True
              _ ->
                return False
    
          when sent $
            recordWithPort deviceName (address nic) i . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
      
    updateCache
      :: MAC
      -> PortNum
      -> UTCTime
      -> STM ()
    updateCache source' portNum timestamp' = do
      av <- readTVar $ portAvailability' V.! (fromIntegral portNum)
      case av of
        Available pd ->
          if status pd == Forwarding || status pd == Learning
            then
              Map.insert (CacheEntry timestamp' portNum) source' cache'
            else
              return ()
        _ ->
          return ()

    stpThread 
      = forever $ do
          NewMessage sourcePort bpdu <- atomically' $ readTQueue notificationQueue'
          case bpdu of
            TopologyChange ->
              undefined
            Configuration msg -> do
              bestMessageUpdated <- atomically' $ do
                let
                  tVar
                    = portAvailability' V.! (fromIntegral sourcePort)
                pa <- readTVar tVar
                case pa of
                  Disabled ->
                    return False
                  Available portData' -> 
                    case configuration portData' of
                      Nothing -> do 
                        writeTVar tVar . Available $ portData' { configuration = Just msg }
                        return True
                      Just msg' ->
                        if msg > msg'
                          then do
                            writeTVar tVar . Available $ portData' { configuration = Just msg }
                            return True
                          else
                            return False
                            
              when bestMessageUpdated $ do
                let
                  getBestMessageByPort msgs (fromIntegral -> portNum') tVar = do 
                    pa <- readTVar tVar
                    case pa of
                      Available (configuration -> Just msg) ->
                        return ((portNum', msg) : msgs)
                      _ ->
                        return msgs
                bestMessages <- atomically' $ V.ifoldM getBestMessageByPort [] portAvailability'
                if null bestMessages
                  then do
                    atomically' $ writeTVar switchStatus' RootSwitch
                  else do 
                    let
                      (rootPort', bestMsg)
                        = minimumBy (comparing snd) bestMessages
                      rootId'
                        = rootId bestMsg
                    if iden' > rootId'
                      then do
                        atomically' $ writeTVar switchStatus' RootSwitch
                      else do
                        let
                          cost'
                            = 1 + rootPathCost bestMsg
                          dummyMessage
                            = ConfigurationMessage False False rootId' cost' iden' 0 0 0 0 0
                          designatedPorts'
                            = Set.fromList . map fst . filter ((dummyMessage >) . snd) $ bestMessages
                          nonRootSwitch
                            = NonRootSwitch' rootPort' rootId' designatedPorts' cost'
                        atomically' . writeTVar switchStatus' $ NonRootSwitch nonRootSwitch
              undefined          
