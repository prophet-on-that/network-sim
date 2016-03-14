{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import NetworkSim.LinkLayer
import qualified NetworkSim.LinkLayer.SimpleNode as SimpleNode
import qualified NetworkSim.LinkLayer.Repeater as Repeater

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Monad.Catch
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.String (fromString)

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
      node0 <- freshMAC >>= atomically . SimpleNode.new
      node1 <- freshMAC >>= atomically . SimpleNode.new
      atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
    
    connectSameT = do
      node0 <- freshMAC >>= atomically . SimpleNode.new
      let
        interface0
          = SimpleNode.interface node0
        handler (ConnectToSelf _)
          = return ()
        handler e
          = throwM e
      handle handler $ do 
        atomically $ connectNICs interface0 interface0
        assertFailure "No exception thrown"

disconnectT :: TestTree
disconnectT
  = testGroup "disconnectPort"
      [ testCase "Disconnecting impossible when already disconnected" disconnectDisconnectedT
      , testCase "Sending impossible after disconnection" noSendT
      , testCase "Mate registers disconnection" noSendT'
      , testCase "Buffer still available after disconnection" bufferAvailableT
      ]
  where
    disconnectDisconnectedT = do
      node0 <- freshMAC >>= atomically . SimpleNode.new
      let
        handler (PortDisconnected _ 0)
          = return ()
        handler e
          = throwM e
      handle handler $ do
        atomically $ disconnectPort (SimpleNode.interface node0) 0
        assertFailure "No exception thrown"
      
    -- | Check disconnecting port can now no longer send.
    noSendT = do
      [mac0, mac1] <- replicateM 2 freshMAC
      node0 <- atomically $ SimpleNode.new mac0
      node1 <- atomically $ SimpleNode.new mac1
      atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
      let
        p0 = do 
          void $ receiveR 0
          interface <- V.head <$> interfacesR
          lift . atomically $ disconnectPort interface 0
          let
            handler (PortDisconnected _ 0)
              = return ()
            handler e
              = throwM e
          handle handler $ do 
            mapReaderT atomically $ sendR "Hello, world" mac1 0 0
            lift $ assertFailure "No exception thrown"

        p1
          = mapReaderT atomically $ sendR "Hello, world" mac0 0 0
      void $ concurrently (runReaderT p0 node0) (runReaderT p1 node1)

    -- | Check mate of disconnected port can now no longer send.
    noSendT' = do 
      [mac0, mac1] <- replicateM 2 freshMAC
      node0 <- atomically $ SimpleNode.new mac0
      node1 <- atomically $ SimpleNode.new mac1
      atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
      signal <- newEmptyTMVarIO
      let
        p0 = do
          interface <- V.head <$> interfacesR
          lift . atomically $ do
            disconnectPort interface 0
            putTMVar signal ()

        p1 = do
          lift . atomically $ takeTMVar signal
          let
            handler (PortDisconnected _ 0)
              = return ()
            handler e
              = throwM e
          handle handler $ do
            mapReaderT atomically $ sendR "Hello, world" mac0 0 0
            lift $ assertFailure "No exception thrown."
      void $ concurrently (runReaderT p0 node0) (runReaderT p1 node1)

    -- | Check buffer contents still available post disconnect
    bufferAvailableT = do 
      [mac0, mac1] <- replicateM 2 freshMAC
      node0 <- atomically $ SimpleNode.new mac0
      node1 <- atomically $ SimpleNode.new mac1
      atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
      signal <- newEmptyTMVarIO
      let
        p0 = do
          interface <- V.head <$> interfacesR
          lift . atomically $ do
            takeTMVar signal
            disconnectPort interface 0
          payload . snd <$> receiveR 0

        message
          = "Hello, world"
            
        p1 = do
          mapReaderT atomically $ sendR message mac0 0 0
          lift . atomically $ putTMVar signal ()
          
      (payload, _) <- concurrently (runReaderT p0 node0) (runReaderT p1 node1)
      assertEqual "Transmitted payload does not equal message" message payload

sendT :: TestTree
sendT
  = testGroup "send"
      [ testCase "Send" simpleSendT
      , testCase "Send and receive" sendAndReceiveT
      ]
  where
    simpleSendT = do
      mac1 <- freshMAC
      node0 <- freshMAC >>= atomically . SimpleNode.new
      node1 <- atomically $ SimpleNode.new mac1
      atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
      let
        message
          = "Hello, world!"

        program0
          = mapReaderT atomically $ sendR message mac1 0 0

        program1
          = payload . snd <$> receiveR 0

      (_, payload) <- concurrently (runReaderT program0 node0) (runReaderT program1 node1)
      assertEqual "Transmitted payload does not equal message" message payload
      
    sendAndReceiveT = do 
      [mac0, mac1] <- replicateM 2 freshMAC
      node0 <- atomically $ SimpleNode.new mac0
      node1 <- atomically $ SimpleNode.new mac1
      atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
      let
        msg0
          = "Hello, world!"

        msg1
          = "Hello, too!"
            
        program0 = do 
          mapReaderT atomically $ sendR msg0 mac1 0 0
          payload . snd <$> receiveR 0

        program1 = do
          msg <- payload . snd <$> receiveR 0
          mapReaderT atomically $ sendR msg1 mac0 0 0
          return msg
            
      (payload0, payload1) <- concurrently (runReaderT program0 node0) (runReaderT program1 node1)
      assertEqual "Payload 0 does not equal message" msg1 payload0
      assertEqual "Payload 1 does not equal message" msg0 payload1

starNetwork
  :: Int -- ^ Number of 'SimpleNode's connected to central repeater. Pre: >= 2.
  -> (MAC -> Int -> Vector MAC -> SimpleNode.Op a) -- ^ Program to run on each 'SimpleNode'. Params: repeater_addr node_number other_addrs. other_addrs is rotated such that first addr is next in sequence.
  -> IO (Vector a)
starNetwork n p = do
  macs <- V.replicateM n freshMAC
  nodes <- atomically $ mapM SimpleNode.new macs
  repeaterMAC <- freshMAC
  repeater <- atomically $ Repeater.new n repeaterMAC
  atomically $ mapM (connectNICs (Repeater.interface repeater) . SimpleNode.interface) nodes
  let
    nodeProgram i node = do
      let
        (before, after)
          = V.splitAt i macs
        otherMACs
          = V.drop 1 after V.++ before
      runReaderT (p repeaterMAC i otherMACs) node

    repeaterProgram
      = runReaderT Repeater.repeater repeater
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
            
        p _ 0 (V.head -> mac) 
          = mapReaderT atomically $ sendR msg0 mac 0 0
        p _ _ _ = do 
          msg <- payload . snd <$> receiveR 0
          lift $ assertEqual "Transmitted payload does not equal message" msg msg0
      void $ starNetwork 2 p

    replicateManyT = do
      let
        n
          = 5
        p _ i (V.head -> mac) = do
          let
            msg
              = fromString . show $ i
          mapReaderT atomically $ sendR msg mac 0 0
          payload . snd <$> receiveR 0
      ret <- starNetwork n p
      assertEqual "Received payloads do not equal transmitted messages" ret (fmap (fromString . show) . V.fromList $ n - 1 : [0 .. n - 2])
