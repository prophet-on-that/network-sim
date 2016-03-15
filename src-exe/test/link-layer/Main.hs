{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import NetworkSim.LinkLayer
import qualified NetworkSim.LinkLayer.SimpleNode as SimpleNode
import qualified NetworkSim.LinkLayer.Repeater as Repeater

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Reader
import Control.Concurrent.Async.Lifted
import Control.Monad.Catch
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.String (fromString)
import Control.Monad.Logger
import Control.Monad.Trans.Control

main
  = defaultMain $ testGroup "Link-layer Tests" 
      [ connectT
      , disconnectT
      , sendT
      , repeaterT
      ]

connectT :: TestTree
connectT
  = testGroup "connectNICs"
      [ testCase "Connect NICs" connectNICsT
      , testCase "Connect same NIC" connectSameT
      ]
  where
    connectNICsT = do 
      node0 <- SimpleNode.new
      node1 <- SimpleNode.new
      runNoLoggingT $ 
        connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
    
    connectSameT = do 
      node0 <- SimpleNode.new
      let
        interface0
          = SimpleNode.interface node0
        handler (ConnectToSelf _)
          = return ()
        handler e
          = throwM e
      handle handler $ do 
        runNoLoggingT $ connectNICs interface0 interface0
        assertFailure "No exception thrown"

disconnectT :: TestTree
disconnectT
  = testGroup "disconnectPort"
      [ testCase "Disconnecting impossible when already disconnected" disconnectDisconnectedT
      -- , testCase "Sending impossible after disconnection" noSendT
      -- , testCase "Mate registers disconnection" noSendT'
      -- , testCase "Buffer still available after disconnection" bufferAvailableT
      ]
  where
    disconnectDisconnectedT = do
      node0 <- SimpleNode.new
      let
        handler (PortDisconnected _ 0)
          = return ()
        handler e
          = throwM e
      handle handler $ do
        runNoLoggingT $ disconnectPort (SimpleNode.interface node0) 0
        assertFailure "No exception thrown"
      
    -- -- | Check disconnecting port can now no longer send.
    -- noSendT = do
    --   [mac0, mac1] <- replicateM 2 freshMAC
    --   node0 <- atomically $ SimpleNode.new mac0
    --   node1 <- atomically $ SimpleNode.new mac1
    --   atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
    --   let
    --     p0 = do 
    --       void receive
    --       interface <- V.head <$> interfacesR
    --       lift . atomically $ disconnectPort interface 0
    --       let
    --         handler (PortDisconnected _ 0)
    --           = return ()
    --         handler e
    --           = throwM e
    --       handle handler $ do 
    --         mapReaderT atomically $ sendR "Hello, world" mac1 0 0
    --         lift $ assertFailure "No exception thrown"

    --     p1
    --       = mapReaderT atomically $ sendR "Hello, world" mac0 0 0
    --   void $ concurrently (runReaderT p0 node0) (runReaderT p1 node1)

    -- -- | Check mate of disconnected port can now no longer send.
    -- noSendT' = do 
    --   [mac0, mac1] <- replicateM 2 freshMAC
    --   node0 <- atomically $ SimpleNode.new mac0
    --   node1 <- atomically $ SimpleNode.new mac1
    --   atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
    --   signal <- newEmptyTMVarIO
    --   let
    --     p0 = do
    --       interface <- V.head <$> interfacesR
    --       lift . atomically $ do
    --         disconnectPort interface 0
    --         putTMVar signal ()

    --     p1 = do
    --       lift . atomically $ takeTMVar signal
    --       let
    --         handler (PortDisconnected _ 0)
    --           = return ()
    --         handler e
    --           = throwM e
    --       handle handler $ do
    --         mapReaderT atomically $ sendR "Hello, world" mac0 0 0
    --         lift $ assertFailure "No exception thrown."
    --   void $ concurrently (runReaderT p0 node0) (runReaderT p1 node1)

    -- -- | Check buffer contents still available post disconnect
    -- bufferAvailableT = do 
    --   [mac0, mac1] <- replicateM 2 freshMAC
    --   node0 <- atomically $ SimpleNode.new mac0
    --   node1 <- atomically $ SimpleNode.new mac1
    --   atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
    --   signal <- newEmptyTMVarIO
    --   let
    --     p0 = do
    --       interface <- V.head <$> interfacesR
    --       lift . atomically $ do
    --         takeTMVar signal
    --         disconnectPort interface 0
    --       payload . snd <$> receiveR 0

    --     message
    --       = "Hello, world"
            
    --     p1 = do
    --       mapReaderT atomically $ sendR message mac0 0 0
    --       lift . atomically $ putTMVar signal ()
          
    --   (payload, _) <- concurrently (runReaderT p0 node0) (runReaderT p1 node1)
    --   assertEqual "Transmitted payload does not equal message" message payload

twoSimpleNodes
  :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m)
  => (Int -> MAC -> SimpleNode.Op m a) -- ^ Program to run on each 'SimpleNode'. Params: index, other_node_addr.
  -> m (a, a)
twoSimpleNodes p = do
  node0 <- SimpleNode.new
  node1 <- SimpleNode.new
  connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
  let
    mac0
      = getMAC . SimpleNode.interface $ node0 
    mac1
      = getMAC . SimpleNode.interface $ node1
  concurrently (SimpleNode.runOp node0 $ p 0 mac1) (SimpleNode.runOp node1 $ p 1 mac0)

sendT :: TestTree
sendT
  = testGroup "send"
      [ testCase "Send" simpleSendT
      , testCase "Send and receive" sendAndReceiveT
      ]
  where
    simpleSendT = do
      let
        message
          = "Hello, world!"
        p 0 mac1 = do 
          SimpleNode.send message mac1
          return mempty
        p _ _ 
          = payload <$> SimpleNode.receive
      (_, result) <- runNoLoggingT $ twoSimpleNodes p
      assertEqual "Transmitted payload does not equal message" message result
      
    sendAndReceiveT = do
      let
        msg0
          = "Hello, world!"
        msg1
          = "Hello, too!"
            
        p 0 mac1 = do 
          SimpleNode.send msg0 mac1
          payload <$> SimpleNode.receive
        p _ mac0 = do
          msg <- payload <$> SimpleNode.receive
          SimpleNode.send msg1 mac0
          return msg
            
      (payload0, payload1) <- runNoLoggingT $ twoSimpleNodes p 
      assertEqual "Payload 0 does not equal message" msg1 payload0
      assertEqual "Payload 1 does not equal message" msg0 payload1

starNetwork
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadThrow m)
  => Int -- ^ Number of 'SimpleNode's connected to central repeater. Pre: >= 2.
  -> (MAC -> Int -> Vector MAC -> SimpleNode.Op m a) -- ^ Program to run on each 'SimpleNode'. Params: repeater_addr node_number other_addrs. other_addrs is rotated such that first addr is next in sequence.
  -> m (Vector a)
starNetwork n p = do
  nodes <- V.replicateM n SimpleNode.new
  let
    macs
      = fmap (getMAC . SimpleNode.interface) nodes
  repeater <- Repeater.new n
  let
    repeaterMAC
      = getMAC . Repeater.interface $ repeater
  mapM (connectNICs (Repeater.interface repeater) . SimpleNode.interface) nodes
  let
    nodeProgram i node = do
      let
        (before, after)
          = V.splitAt i macs
        otherMACs
          = V.drop 1 after V.++ before
      SimpleNode.runOp node $
        p repeaterMAC i otherMACs

    repeaterProgram
      = Repeater.runOp repeater Repeater.repeater
  withAsync repeaterProgram $ \_ -> 
    mapConcurrently id $ V.imap nodeProgram nodes
  
repeaterT :: TestTree
repeaterT
  = testGroup "repeater"
      [ testCase "Replicate" replicateT
      , testCase "Replicate many" replicateManyT
      ]
  where
    replicateT = do
      let
        msg0
          = "Hello, world!"
            
        p _ 0 (V.head -> mac) = do 
          SimpleNode.send msg0 mac
          return mempty
        p _ _ _ 
          = payload <$> SimpleNode.receive
      results <- runNoLoggingT $ starNetwork 2 p
      assertEqual "Transmitted payload does not equal message" msg0 (results V.! 1)

    replicateManyT = do
      let
        n
          = 5
        p _ i (V.head -> mac) = do
          let
            msg
              = fromString . show $ i
          SimpleNode.send msg mac
          payload <$> SimpleNode.receive
      ret <- runStderrLoggingT $ starNetwork n p
      assertEqual "Received payloads do not equal transmitted messages" ret (fmap (fromString . show) . V.fromList $ n - 1 : [0 .. n - 2])
