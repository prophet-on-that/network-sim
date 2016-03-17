{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

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
import Control.Concurrent.STM.Lifted

defaultMessage
  = "Hello, world!"
  
main
  = defaultMain $ testGroup "Link-layer Tests" 
      [ testGroup "NIC"
          [ connectT
          , sendT
          , broadcastT
          , promiscuityT
          , disconnectT
          ]
      , repeaterT
      ]

connectT :: TestTree
connectT
  = testGroup "Connecting"
      [ testCase "Connect NICs" connectNICsT
      , testCase "Connect multiple NICs" connectMultipleT
      , testCase "Connecting fails when no free ports available" catchNoFreePortsT
      , testCase "Connecting same NIC fails" connectSameT
      ]
  where
    connectNICsT 
      = runNoLoggingT $ do 
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1

    connectMultipleT
      = runNoLoggingT $ do
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          node2 <- newNIC 2 False
          connectNICs node0 node2
          connectNICs node1 node2

    catchNoFreePortsT
      = runNoLoggingT $ do
          node0 <- newNIC 0 False
          node1 <- newNIC 1 False
          let
            handler (NoFreePort _)
              = return ()
            handler e
              = throwM e
          handle handler $ do 
            connectNICs node0 node1
            liftIO $ assertFailure "No exception thrown"
    
    connectSameT
      = runNoLoggingT $ do
          node0 <- newNIC 2 False
          let
            handler (ConnectToSelf _)
              = return ()
            handler e
              = throwM e
          handle handler $ do 
            connectNICs node0 node0
            liftIO $ assertFailure "No exception thrown"

sendT :: TestTree
sendT
  = testGroup "Transmitting"
      [ testCase "Send and receive" sendAndReceiveT
      , testCase "Receive and reply" receiveAndReplyT
      ]
  where
    sendAndReceiveT
      = runNoLoggingT $ do 
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          let
            frame
              = Frame (address node1) (address node0) defaultMessage
          (_, payload . snd -> result) <- concurrently
                           (atomically $ sendOnNIC frame node0 0)
                           (atomically $ receiveOnNIC node1)
          liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result
      
    receiveAndReplyT
      = runNoLoggingT $ do 
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          let
            addr0
              = address node0
            addr1
              = address node1
            msg0
              = "Hello, world!"
            msg1
              = "Hello, too!"
                
            prog0 = do
              let
                frame
                  = Frame addr1 addr0 msg0
              atomically $ sendOnNIC frame node0 0
              fmap (payload . snd) . atomically $ receiveOnNIC node0
              
            prog1 = do
              msg <- fmap (payload . snd) . atomically $ receiveOnNIC node1
              let
                frame
                  = Frame addr0 addr1 msg1
              atomically $ sendOnNIC frame node1 0
              return msg

          (payload0, payload1) <- concurrently prog0 prog1
          liftIO $ do
            assertEqual "Payload 0 does not equal message" msg1 payload0
            assertEqual "Payload 1 does not equal message" msg0 payload1

broadcastT :: TestTree
broadcastT
  = testGroup "Broadcast"
      [ testCase "NIC receives broadcast message" . runNoLoggingT $ do
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          let
            frame
              = Frame broadcastAddr (address node0) defaultMessage
          atomically $ sendOnNIC frame node0 0
          inFrame <- fmap snd . atomically $ receiveOnNIC node1
          liftIO $ do
            assertEqual "Transmitted frame is not identified as a broadcast" Broadcast (destination inFrame)
            assertEqual "Received payload does not equal message" defaultMessage (payload inFrame)
      ]

promiscuityT :: TestTree
promiscuityT
  = testGroup "Promiscuous mode"
      [ testCase "Non-promiscuous NIC ignores frame with other address" . runNoLoggingT $ do
          addr <- liftIO freshMAC
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          let
            frame
              = Frame addr (address node0) defaultMessage
            frame'
              = frame { destination = address node1 } 
          atomically $ do
            sendOnNIC frame node0 0
            sendOnNIC frame' node0 0 
          inFrame <- fmap snd . atomically $ receiveOnNIC node1
          liftIO $ assertEqual "Expect frame with other destination to be dropped" (MAC $ address node1) (destination inFrame)

      , testCase "Promiscuous NIC accepts frame with other address" . runNoLoggingT $ do
          addr <- liftIO freshMAC
          node0 <- newNIC 1 False
          node1 <- newNIC 1 True
          connectNICs node0 node1
          let
            frame
              = Frame addr (address node0) defaultMessage
          atomically $ sendOnNIC frame node0 0
          void . atomically $ receiveOnNIC node1

      , testCase "Enabling promiscuous mode functions correctly" . runNoLoggingT $ do
          addr <- liftIO freshMAC
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          setPromiscuity node1 True
          connectNICs node0 node1
          let
            frame
              = Frame addr (address node0) defaultMessage
          atomically $ sendOnNIC frame node0 0
          void . atomically $ receiveOnNIC node1
          
      , testCase "Disabling promiscuous mode functions correctly" . runNoLoggingT $ do
          addr <- liftIO freshMAC
          node0 <- newNIC 1 False
          node1 <- newNIC 1 True
          setPromiscuity node1 False
          connectNICs node0 node1
          let
            frame
              = Frame addr (address node0) defaultMessage
            frame'
              = frame { destination = address node1 } 
          atomically $ do
            sendOnNIC frame node0 0
            sendOnNIC frame' node0 0 
          inFrame <- fmap snd . atomically $ receiveOnNIC node1
          liftIO $ assertEqual "Expect frame with other destination to be dropped" (MAC $ address node1) (destination inFrame)
      ]

disconnectT :: TestTree
disconnectT
  = testGroup "Disconnecting"
      [ testCase "Disconnecting impossible when already disconnected" disconnectDisconnectedT
      , testCase "Sending impossible after disconnection" noSendT
      , testCase "Mate registers disconnection" mateUnregisteredT
      , testCase "Buffer still available after disconnection" bufferAvailableT
      ]
  where
    disconnectDisconnectedT
      = runNoLoggingT $ do 
          node0 <- newNIC 1 False
          let
            handler (PortDisconnected _ 0)
              = return ()
            handler e
              = throwM e
          handle handler $ do
            disconnectPort node0 0
            liftIO $ assertFailure "No exception thrown"
      
    -- | Check disconnected port can now no longer send.
    noSendT
      = runNoLoggingT $ do
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          disconnectPort node0 0
          let
            frame
              = Frame (address node1) (address node0) defaultMessage
            handler (PortDisconnected _ _)
              = return ()
            handler e
              = throwM e
          handle handler $ do 
            atomically $ sendOnNIC frame node0 0
            liftIO $ assertFailure "No exception thrown"

    -- | Check mate can no longer send after disconnect.
    mateUnregisteredT
      = runNoLoggingT $ do 
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          disconnectPort node0 0
          let
            frame
              = Frame (address node0) (address node1) defaultMessage
            handler (PortDisconnected _ _)
              = return ()
            handler e
              = throwM e
          handle handler $ do 
            atomically $ sendOnNIC frame node1 0
            liftIO $ assertFailure "No exception thrown"
          
    -- | Check buffer contents still available post disconnect
    bufferAvailableT
      = runNoLoggingT $ do
          node0 <- newNIC 1 False
          node1 <- newNIC 1 False
          connectNICs node0 node1
          let
            frame
              = Frame (address node1) (address node0) defaultMessage
          atomically $ sendOnNIC frame node0 0 
          disconnectPort node1 0
          result <- fmap (payload . snd) . atomically $ receiveOnNIC node1
          liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result

starNetwork
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadThrow m)
  => Int -- ^ Number of 'SimpleNode's connected to central repeater. Pre: >= 2.
  -> (MAC -> Int -> Vector MAC -> SimpleNode.Op m a) -- ^ Program to run on each 'SimpleNode'. Params: repeater_addr node_number other_addrs. other_addrs is rotated such that first addr is next in sequence.
  -> m (Vector a)
starNetwork n p = do
  nodes <- V.replicateM n SimpleNode.new
  let
    macs
      = fmap (address . SimpleNode.interface) nodes
  repeater <- Repeater.new n
  let
    repeaterMAC
      = address . Repeater.interface $ repeater
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
  = testGroup "Repeater"
      [ testCase "Replicate" replicateT
      , testCase "Replicate many" replicateManyT
      , testCase "Repeater picks up own message" . runNoLoggingT $ do
          node0 <- newNIC 1 False
          repeater <- Repeater.new 1
          connectNICs node0 (Repeater.interface repeater)
          let
            frame
              = Frame (address . Repeater.interface $ repeater) (address node0) defaultMessage
          atomically $ sendOnNIC frame node0 0
          result <- Repeater.runOp repeater $ 
            payload . snd <$> Repeater.receive
          liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result

      , testCase "Message replicated several times" . runNoLoggingT $ do
          node0 <- newNIC 1 False 
          node1 <- newNIC 1 False
          repeater0 <- Repeater.new 2
          repeater1 <- Repeater.new 2
          connectNICs node0 (Repeater.interface repeater0)
          connectNICs (Repeater.interface repeater0) (Repeater.interface repeater1)
          connectNICs (Repeater.interface repeater1) node1
          
          withAsync (Repeater.runOp repeater0 Repeater.repeater) . const $
            withAsync (Repeater.runOp repeater1 Repeater.repeater) . const $ do 
              let
                frame
                  = Frame (address node1) (address node0) defaultMessage
              atomically $ sendOnNIC frame node0 0
              result <- fmap (payload . snd) . atomically $ receiveOnNIC node1
              liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result
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
      ret <- runNoLoggingT $ starNetwork n p
      assertEqual "Received payloads do not equal transmitted messages" ret (fmap (fromString . show) . V.fromList $ n - 1 : [0 .. n - 2])
