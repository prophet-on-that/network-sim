-- | This module is intended to be imported qualified.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module NetworkSim.LinkLayer.Switch.STP
  ( Priority
  , defaultPriority
  , Switch ()
  , interface
  , switchHelloTime
  , defaultHelloTime
  , switchForwardDelay
  , defaultForwardDelay
  , switchMaxAge
  , defaultMaxAge
  , new
  , run
  ) where

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
import Data.Fixed
import Control.Concurrent (threadDelay)
import GHC.Exts (groupWith)
import Data.List (sort, intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad.Catch

deviceName
  = "STP Switch"

type Priority = Word16

defaultPriority :: Priority
defaultPriority
  = 80

data SwitchId = SwitchId
  { priority :: {-# UNPACK #-} !Priority
  , switchAddr :: {-# UNPACK #-} !MAC
  } deriving (Eq, Generic, Binary)

instance Show SwitchId where
  show (SwitchId priority' switchAddr')
    = show priority' <> "." <> show switchAddr'

instance Ord SwitchId where
  compare sid sid'
    | comp == EQ
        = comparing (Down . switchAddr) sid sid'
    | otherwise
        = comp
    where
      comp
        = comparing (Down . priority) sid sid'
          
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
    = if c0 == EQ
        then
          if c1 == EQ
            then
              if c2 == EQ
                then
                  c3
                else
                  c2
            else
              c1
        else
          c0
    where
      c0
        = comparing rootId msg msg'
      c1 
        = comparing (Down . rootPathCost) msg msg'
      c2
        = comparing switchId msg msg'
      c3 
        = comparing (Down . portId) msg msg'

-- | Alternative to Show instance which is a shorter representation of
-- the 'ConfigurationMessage'.
showConfigurationMessage
  :: ConfigurationMessage
  -> T.Text
showConfigurationMessage msg
  = T.intercalate "/"
      [ "root: " <> (T.pack . show . rootId) msg
      , "cost: " <> (T.pack . show . rootPathCost) msg
      , "age: " <> (T.pack . show . messageAge) msg
      , "maxAge: " <> (T.pack . show . maxAge) msg
      , "helloTime: " <> (T.pack . show . helloTime) msg
      , "forwardDelay: " <> (T.pack . show . forwardDelay) msg
      ]

data PortStatus
  = Blocked
  | Listening {-# UNPACK #-} !UTCTime
  | Learning {-# UNPACK #-} !UTCTime
  | Forwarding
  deriving (Show, Eq, Ord)

portStatusStr
  :: PortStatus
  -> T.Text
portStatusStr Blocked
  = "Blocked"
portStatusStr (Listening _)
  = "Listening"
portStatusStr (Learning _)
  = "Learning"
portStatusStr Forwarding
  = "Forwarding"

isListeningStatus (Listening _)
  = True
isListeningStatus _
  = False

isLearningStatus (Learning _)
  = True
isLearningStatus _
  = False

data PortData = PortData
  { status :: !PortStatus
  , configuration :: !(Maybe ConfigurationMessage)
  } deriving (Show)

data PortAvailability
  = Disabled
  | Available {-# UNPACK #-} !PortData
  deriving (Show)

data SwitchStatus
  = RootSwitch
  | NonRootSwitch {-# UNPACK #-} !NonRootSwitch
  deriving (Eq)

data NonRootSwitch = NonRootSwitch'
  { rootPort :: {-# UNPACK #-} !PortNum
  , rootPortId :: {-# UNPACK #-} !SwitchId
  , designatedPorts :: !(Set PortNum)
  , cost :: {-# UNPACK #-} !Word32
  } deriving (Eq)

-- When broadcasting, attempt to send on ports labelled 'Disabled' in
-- case of out of date. If ever receive 'PortDisconnected' exception,
-- switch port status to 'Disabled' and commence topology change
-- procedure.

data Notification
  = NewMessage {-# UNPACK #-} !PortNum !BPDU
  | Hello
  | Recompute

data CacheEntry = CacheEntry
  { timestamp :: {-# UNPACK #-} !UTCTime -- ^ Time at which the entry was installed into the cache.
  , portNum :: {-# UNPACK #-} !PortNum
  }

data Switch = Switch 
  { interface :: {-# UNPACK #-} !NIC
  , portAvailability :: {-# UNPACK #-} !(Vector (TVar PortAvailability))
  , cache :: !(Map MAC CacheEntry)
  , notificationQueue :: !(TQueue Notification)
  , switchPriority :: {-# UNPACK #-} !Priority
  , switchStatus :: !(TVar SwitchStatus)
  , lastHello :: !(TVar (Maybe UTCTime))
  , switchHelloTime :: !(TVar Word16) -- ^ Measured in 1/256 seconds.
  , switchForwardDelay :: !(TVar Word16) -- ^ Measured in 1/256 seconds.
  , switchMaxAge :: !(TVar Word16) -- ^ Measured in 1/256 seconds.
  }

iden
  :: Switch
  -> SwitchId
iden (Switch {..})
  = SwitchId switchPriority (address interface)

defaultHelloTime :: Word16
defaultHelloTime
  = 512

defaultForwardDelay :: Word16
defaultForwardDelay
  = 3840

defaultMaxAge :: Word16
defaultMaxAge
  = 5120 -- 20s.

new
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Word16 -- ^ Number of ports. Pre: positive.
  -> Priority -- ^ Switch priority. See also 'defaultPriority'.
  -> m Switch
new n prio = do
  nic <- newNIC n True
  announce $ "Creating new STP Switch with address " <> (T.pack . show . address) nic
  atomically' $ Switch nic
    <$> (V.replicateM (fromIntegral n) $ newTVar Disabled)
    <*> Map.new
    <*> newTQueue
    <*> return prio
    <*> newTVar RootSwitch
    <*> newTVar Nothing
    <*> newTVar defaultHelloTime
    <*> newTVar defaultForwardDelay
    <*> newTVar defaultMaxAge

-- | The status of the 'Switch' is re-initialised with each 'run'.
run
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadCatch m)
  => Switch
  -> m ()
run switch@(Switch nic portAvailability' cache' notificationQueue' _ switchStatus' lastHello' switchHelloTime' switchForwardDelay' switchMaxAge') = do 
  initialise
  withAsync timer . const $ 
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
    iden'
      = iden switch
        
    portCount'
      = portCount nic

    portIndices
      = [0 .. portCount' - 1]
        
    -- A wrapper around 'sendOnNIC' that ensure the port is in the
    -- 'Forwarding' state before sending, and to handle a port
    -- disconnection. The return value indicates if the value was
    -- actually send.
    sendOnPort frame i = do
      let
        tVar
          = portAvailability' V.! (fromIntegral i)
      av <- atomically' $ readTVar tVar
      case av of
        Available (status -> Forwarding) -> do
          let
            handler (PortDisconnected _ _) = do 
              atomically' $ do
                writeTVar tVar Disabled
                writeTQueue notificationQueue' Recompute
              recordWithPort deviceName (address nic) i "Exception raised when sending. Setting port to disabled."
              return False
            handler e
              = throwM e
                
          handle handler $ do 
            atomically' $ sendOnNIC frame nic i
            return True
        _ ->
          return False
    
    initialise = do
      atomically' initialise'
      logStatus
      where
        initialise' :: STM ()
        initialise' = do 
          initialisePortAvailability
          writeTVar switchStatus' RootSwitch
          setPortConnectHook onPortConnect nic
          where
            initialisePortAvailability = do
              portInfo' <- portInfo nic
              V.forM_ (V.indexed portInfo') $ \(i, info) ->
                writeTVar (portAvailability' V.! i) $
                  if isConnected info
                    then
                      Available $ PortData Forwarding Nothing
                    else
                      Disabled

            onPortConnect :: PortConnectHook
            onPortConnect _ portNum _ = do 
              writeTVar (portAvailability' V.! (fromIntegral portNum)) (Available (PortData Blocked Nothing))
              writeTQueue notificationQueue' Recompute

        logStatus = do
          let
            readPort ports i tv = do
              av <- readTVar tv
              case av of
                Available _ ->
                  return $ i : ports
                _ ->
                  return ports 
          forwardingPorts <- atomically' $ V.ifoldM' readPort [] portAvailability'
          record deviceName (switchAddr iden') $ "Initialising as root switch with forwarding ports: " <> (T.intercalate ", " . map (T.pack . show) . sort) forwardingPorts
              
    -- Forward broadcast frame on ports in 'Forwarding' state.
    broadcast originPort frame
      = void $ mapConcurrently forward indices
      where
        indices
          = filter (/= originPort) portIndices
        dest
          = destinationAddr . destination $ frame
        outFrame
          = frame { destination = dest }

        forward i = do
          sent <- sendOnPort outFrame i
          when sent $
            recordWithPort deviceName (address nic) i . T.pack $ "Forwarding frame from " <> (show . source) frame <> " to " <> show dest
      
    updateCache
      :: MAC
      -> PortNum
      -> UTCTime
      -> STM ()
    updateCache source' portNum timestamp' = do
      let
        tvar
          = portAvailability' V.! (fromIntegral portNum)
      av <- readTVar tvar
      case av of
        Available pd ->
          if status pd == Forwarding || (isLearningStatus . status) pd
            then
              Map.insert (CacheEntry timestamp' portNum) source' cache'
            else
              return ()
        Disabled -> 
          writeTVar tvar $ Available (PortData Blocked Nothing)
          -- TODO: inform switch about topology change.

    stpThread 
      = forever $ do
          notification' <- atomically' $ readTQueue notificationQueue'
          case notification' of
            Hello -> do
              ss <- atomically' $ readTVar switchStatus'
              case ss of
                RootSwitch -> do
                  (helloTime', forwardDelay', maxAge') <- atomically' $ (,,)
                    <$> readTVar switchHelloTime'
                    <*> readTVar switchForwardDelay'
                    <*> readTVar switchMaxAge'
                  void $ forConcurrently portIndices $ \i -> do
                    let
                      configurationMsg
                        = ConfigurationMessage False False iden' 0 iden' i 0 maxAge' helloTime' forwardDelay'
                      frame
                        = Frame stpAddr (switchAddr iden') . encode $ Configuration configurationMsg
                    sent <- sendOnPort frame i
                    when sent $
                      recordWithPort deviceName (switchAddr iden') i $ "Sending Hello: " <> showConfigurationMessage configurationMsg
                    
                _ ->
                  return ()
              
            NewMessage sourcePort bpdu ->
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
                            if msg > msg' || messageAge msg < messageAge msg'
                              then do
                                writeTVar tVar . Available $ portData' { configuration = Just msg }
                                return True
                              else
                                return False

                  when bestMessageUpdated recompute
              
                  ss <- atomically' $ readTVar switchStatus'
                  case ss of
                    RootSwitch ->
                      -- TODO: further behaviour may be required here.
                      return ()
                    NonRootSwitch info ->
                      if rootId msg == rootPortId info
                        then
                          forM_ (designatedPorts info) $ \i -> do
                            let
                              configurationMsg
                                = ConfigurationMessage False False (rootPortId info) (cost info) iden' i 0 (maxAge msg) (helloTime msg) (forwardDelay msg)
                              frame
                                = Frame stpAddr (switchAddr iden') . encode $ Configuration configurationMsg
                            sent <- sendOnPort frame i
                            when sent $ 
                              recordWithPort deviceName (switchAddr iden') i $ "Sending configuration: " <> showConfigurationMessage configurationMsg
                        else
                          return ()
                          
            Recompute ->
              recompute
      where
        -- 'recompute' is an atomic operation which requires an
        -- IO-based monadic type for logging only.
        recompute = do 
          now <- liftIO getCurrentTime
          (ss, statusChanges) <- atomically' $ do
            ssOld <- readTVar switchStatus' 
            statusChanges <- recompute' now
            ss <- readTVar switchStatus'
            return (mfilter (/= ssOld) $ Just ss, statusChanges)

          -- Log switch status.
          case ss of
            Just RootSwitch ->
              record deviceName (switchAddr iden') $ "STP recompute, switch is root"
            Just (NonRootSwitch (NonRootSwitch' rootPort' rootPortId' designatedPorts' cost')) -> do 
              let
                str
                  = if Set.null designatedPorts'
                      then
                        "none"
                      else
                        T.intercalate ", " . map (T.pack . show) . Set.toList $ designatedPorts'
              record deviceName (switchAddr iden') $ "STP recompute, root is " <> (T.pack . show) rootPortId' <> " on local port #" <> (T.pack . show) rootPort' <> ", cost " <> (T.pack . show) cost' <> ". Designated ports: " <> str <> "."
            Nothing ->
              return ()
          
          logPortStatusChanges statusChanges
          where
            recompute'
              :: UTCTime
              -> STM [(PortNum, PortStatus)]
            recompute' now = do
              messages <- V.ifoldM getMessageByPort [] portAvailability'
              let
                bestMessages
                  = [(i, msg) | (i, Just msg) <- messages]
              if null bestMessages
                then do
                  writeTVar switchStatus' RootSwitch
                  unblockPorts portIndices
                else do 
                  let
                    (rootPort', bestMsg)
                      = maximumBy (comparing snd) bestMessages
                    rootId'
                      = rootId bestMsg
                  if iden' >= rootId'
                    then do
                      writeTVar switchStatus' RootSwitch
                      unblockPorts portIndices
                    else do
                      let
                        cost'
                          = 1 + rootPathCost bestMsg
                        dummyMessage
                          = ConfigurationMessage False False rootId' cost' iden' 0 0 0 0 0
                        designatedPorts'
                          = Set.fromList . map fst . filter (fromMaybe True . fmap (dummyMessage >) . snd) $ messages
                        nonRootSwitch
                          = NonRootSwitch' rootPort' rootId' designatedPorts' cost'
                      writeTVar switchStatus' $ NonRootSwitch nonRootSwitch
              
                      let
                        portsToBlock
                          = filter (/= rootPort') . filter (not . flip Set.member designatedPorts') $ portIndices
                      blocked <- blockPorts portsToBlock
                      unblocked <- unblockPorts $ Set.toList designatedPorts'
                      return $ unblocked ++ blocked
              where
                -- Unblock blocked ports, and return list of unblocked
                -- ports.
                unblockPorts
                  :: [PortNum]
                  -> STM [(PortNum, PortStatus)]
                unblockPorts
                  = foldlM helper []
                  where
                    helper unblocked i = do
                      let
                        tVar
                          = portAvailability' V.! (fromIntegral i)
                      av <- readTVar tVar
                      case av of
                        Available pd@(status -> Blocked) -> do 
                          writeTVar tVar . Available $ pd { status = Listening now }
                          return $ (i, Listening now) : unblocked
                        _ ->
                          return unblocked

                -- Block unblocked ports, and return list of blocked
                -- ports.
                blockPorts
                  :: [PortNum]
                  -> STM [(PortNum, PortStatus)]
                blockPorts
                  = foldlM helper []
                  where
                    helper blocked i = do
                      let
                        tVar
                          = portAvailability' V.! (fromIntegral i)
                      av <- readTVar tVar
                      case av of
                        Available pd ->
                          if status pd /= Blocked
                            then do 
                              writeTVar tVar . Available $ pd { status = Blocked }
                              return $ (i, Blocked) : blocked
                           else
                             return blocked
                        _ ->
                          return blocked
                
                getMessageByPort msgs (fromIntegral -> portNum') tVar = do 
                  pa <- readTVar tVar
                  case pa of
                    Available (configuration -> msg) ->
                      return ((portNum', msg) : msgs)
                    _ ->
                      return msgs
 
    logPortStatusChanges statusChanges
      = when (not . null $ statusChanges) $ do
          let
            printedChanges
              = map printChanges $ groupWith snd statusChanges
              where
                printChanges changes
                  = (T.intercalate ", " . map (T.pack . show)) affectedPorts <> " to " <> portStatusStr newStatus
                  where
                    newStatus
                      = snd . head $ changes
                    affectedPorts
                      = sort $ map fst changes
          record deviceName (switchAddr iden') $ "Updating port statuses: " <> T.intercalate ", " printedChanges

    timer
      = forever $ do
          now <- liftIO getCurrentTime
          atomically' $ checkHelloDue now
          updatePortStatus now
          ageConfigurationMessages
          sleep
      where
        -- multiples of 1/256 seconds.
        period
          = 1
            
        checkHelloDue now = do 
          len <- readTVar switchHelloTime'
          t <- readTVar lastHello'
          let
            helloDue
              = maybe True ((now >=) . addUTCTime (word16ToNominalDiffTime len)) t
          when helloDue $ do 
            writeTQueue notificationQueue' Hello
            writeTVar lastHello' $ Just now

        updatePortStatus now
          = atomically' updatePortStatus' >>= logPortStatusChanges
          where
            updatePortStatus' = do 
              forwardDelay' <- readTVar switchForwardDelay'
              let
                update updated (fromIntegral -> i) tv = do
                  av <- readTVar tv
                  case av of
                    Available pd@(status -> Listening t) -> do
                      let
                        t'
                          = addUTCTime (word16ToNominalDiffTime forwardDelay') t
                      if now >= t' 
                        then do
                          writeTVar tv . Available $ pd { status = Learning t' }
                          return $ (i, Learning t') : updated
                        else do
                          return updated
                    Available pd@(status -> Learning t) -> 
                      if now >= addUTCTime (word16ToNominalDiffTime forwardDelay') t
                        then do
                          writeTVar tv . Available $ pd { status = Forwarding }
                          return $ (i, Forwarding) : updated
                        else do
                          return updated
                    _ ->
                      return updated
              
              V.ifoldM' update [] portAvailability'

        ageConfigurationMessages = do 
          erased <- atomically' $ V.ifoldM' update [] portAvailability'
          when (not . null $ erased) $ do
            atomically' $ writeTQueue notificationQueue' Recompute
            record deviceName (switchAddr iden') . T.pack $
              "Erasing old configuration messages on port(s) " <> intercalate ", " (map show erased)
          where
            update erased (fromIntegral -> i) tv = do
              av <- readTVar tv
              case av of
                Available pd@(configuration -> Just msg) -> do
                  let
                    newAge
                      = messageAge msg + period
                  if newAge >= maxAge msg
                    then do
                      writeTVar tv $ Available pd { configuration = Nothing }
                      return $ i : erased
                    else do
                      let
                        updatedMsg
                          = msg { messageAge = newAge }
                      writeTVar tv $ Available pd { configuration = Just updatedMsg }
                      return erased

                _ ->
                  return erased
              
        sleep
          = liftIO $ do 
              now <- getCurrentTime
              let
                remainder
                  = utctDayTime now `mod'` (fromIntegral period * 0.00390625)
              threadDelay . truncate $ remainder * 1000000

word16ToNominalDiffTime
  :: Word16
  -> NominalDiffTime
word16ToNominalDiffTime
  = (/ 256) . fromIntegral 
