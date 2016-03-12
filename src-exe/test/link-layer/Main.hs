{-# LANGUAGE OverloadedStrings #-}

module Main where

import NetworkSim.LinkLayer
import qualified NetworkSim.LinkLayer.SimpleNode as SimpleNode

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Monad.Catch
import qualified Data.Vector as V

main
  = defaultMain $ testGroup "Link-layer Tests" 
      [ connectT
      , disconnectT
      , sendT
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
sendT = testCase "send" $ do
  mac1 <- freshMAC
  node0 <- freshMAC >>= atomically . SimpleNode.new
  node1 <- atomically $ SimpleNode.new mac1
  atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)
  let
    message
      = "Hello, world!"
        
    program0 :: SimpleNode.Op ()
    program0
      = mapReaderT atomically $ sendR message mac1 0 0

    program1 :: SimpleNode.Op LB.ByteString
    program1
      = payload . snd <$> receiveR 0
        
  (_, payload) <- concurrently (runReaderT program0 node0) (runReaderT program1 node1)
  assertEqual "Transmitted payload does not equal message" message payload
