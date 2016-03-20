{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import NetworkSim.LinkLayer
import NetworkSim.LinkLayer.SimpleNode (SimpleNode)
import qualified NetworkSim.LinkLayer.SimpleNode as SimpleNode
import qualified NetworkSim.LinkLayer.Hub as Hub
import NetworkSim.LinkLayer.Switch (Switch)
import qualified NetworkSim.LinkLayer.Switch as Switch

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
import Data.Foldable

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
      , hubT
      , switchT
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
          liftIO $ assertEqual "Expect frame with other destination to be dropped" (Unicast $ address node1) (destination inFrame)

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
          liftIO $ assertEqual "Expect frame with other destination to be dropped" (Unicast $ address node1) (destination inFrame)
      ]

disconnectT :: TestTree
disconnectT
  = testGroup "Disconnecting"
      [ testCase "Disconnecting impossible when already disconnected" disconnectDisconnectedT
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
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
  => Int -- ^ Number of 'SimpleNode's connected to central hub. Pre: >= 2.
  -> (MAC -> Int -> Vector MAC -> SimpleNode.Op m a) -- ^ Program to run on each 'SimpleNode'. Params: hub_addr node_number other_addrs. other_addrs is rotated such that first addr is next in sequence.
  -> m (Vector a)
starNetwork n p = do
  nodes <- V.replicateM n SimpleNode.new
  let
    macs
      = fmap (address . SimpleNode.interface) nodes
  hub <- Hub.new n
  let
    hubMAC
      = address . Hub.interface $ hub
  mapM (connectNICs (Hub.interface hub) . SimpleNode.interface) nodes
  let
    nodeProgram i node = do
      let
        (before, after)
          = V.splitAt i macs
        otherMACs
          = V.drop 1 after V.++ before
      SimpleNode.runOp node $
        p hubMAC i otherMACs

    hubProgram
      = Hub.runOp hub Hub.hub
  withAsync hubProgram $ \_ -> 
    mapConcurrently id $ V.imap nodeProgram nodes
  
hubT :: TestTree
hubT
  = testGroup "Hub"
      [ testCase "Replicate" replicateT
      , testCase "Replicate many" replicateManyT
      , testCase "Hub picks up own message" . runNoLoggingT $ do
          node0 <- newNIC 1 False
          hub <- Hub.new 1
          connectNICs node0 (Hub.interface hub)
          let
            frame
              = Frame (address . Hub.interface $ hub) (address node0) defaultMessage
          atomically $ sendOnNIC frame node0 0
          result <- Hub.runOp hub $ 
            payload . snd <$> Hub.receive
          liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result

      , testCase "Message replicated several times" . runNoLoggingT $ do
          node0 <- newNIC 1 False 
          node1 <- newNIC 1 False
          hub0 <- Hub.new 2
          hub1 <- Hub.new 2
          connectNICs node0 (Hub.interface hub0)
          connectNICs (Hub.interface hub0) (Hub.interface hub1)
          connectNICs (Hub.interface hub1) node1
          
          withAsync (Hub.runOp hub0 Hub.hub) . const $
            withAsync (Hub.runOp hub1 Hub.hub) . const $ do 
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

switchStarNetwork
  :: (MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
  => Int
  -> m (Switch, [SimpleNode])
switchStarNetwork n = do
  nodes <- replicateM n SimpleNode.new
  switch <- Switch.new n
  mapM (connectNICs (Switch.interface switch) . SimpleNode.interface) nodes
  return (switch, nodes)

switchT :: TestTree
switchT
  = testGroup "Switch"
      [ testCase "Single message forwarded correctly" . runNoLoggingT $ do
          (switch, [node0, node1]) <- switchStarNetwork 2
          withAsync (Switch.runOp switch Switch.switch) . const $ do
            SimpleNode.runOp node0 $ SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
            result <- payload <$> SimpleNode.runOp node1 SimpleNode.receive
            liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result

      , testCase "Broadcast always forwarded" . runNoLoggingT $ do
           let
             n
               = 5
           (switch, nodes) <- switchStarNetwork n
           withAsync (Switch.runOp switch Switch.switch) . const $ do
             let
               prog = do 
                 SimpleNode.send defaultMessage broadcastAddr
                 replicateM_ (n - 1) $ void SimpleNode.receive
             runConcurrently . sequenceA_ . map (Concurrently . flip SimpleNode.runOp prog) $ nodes

      , testCase "Switch learns port of host" . runNoLoggingT $ do
          (switch, [node0, node1, node2]) <- switchStarNetwork 3
          setPromiscuity (SimpleNode.interface node2) True
          withAsync (Switch.runOp switch Switch.switch) . const $ do
            let
              prog0 = do
                SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                SimpleNode.receive
              prog1 = do
                void SimpleNode.receive
                SimpleNode.send defaultMessage (address . SimpleNode.interface $ node0)
                SimpleNode.send defaultMessage broadcastAddr
              prog2 = do
                void SimpleNode.receive
                SimpleNode.receive
                
            (frame0, _, frame2) <- runConcurrently $ (,,)
              <$> Concurrently (SimpleNode.runOp node0 prog0)
              <*> Concurrently (SimpleNode.runOp node1 prog1)
              <*> Concurrently (SimpleNode.runOp node2 prog2)
                                   
            liftIO $ do
              assertEqual "Transmitted payload does not equal message" defaultMessage (payload frame0)
              assertEqual "node2 should not receive frame destined for node0" Broadcast (destination frame2)

      , testCase "Switch receives message to self" . runNoLoggingT $ do
          (switch, [node0]) <- switchStarNetwork 1
          SimpleNode.runOp node0 $ SimpleNode.send defaultMessage (address . Switch.interface $ switch)
          frame <- Switch.runOp switch $
            snd <$> Switch.receive
          liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage (payload frame)

      , testCase "Switch recovers on host reconnect" . runNoLoggingT $ do
          [node0, node1, node2] <- replicateM 3 SimpleNode.new
          switch <- Switch.new 3
          let
            switchNIC
              = Switch.interface switch
          connectNICs switchNIC (SimpleNode.interface node0)
          connectNICs switchNIC (SimpleNode.interface node1)
          withAsync (Switch.runOp switch Switch.switch) . const $ do
            void $ runConcurrently $ (,) 
              <$> ( Concurrently . SimpleNode.runOp node0 $ do 
                      SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                      void SimpleNode.receive
                      
                  )
              <*> ( Concurrently . SimpleNode.runOp node1 $ do
                      void SimpleNode.receive
                      SimpleNode.send defaultMessage (address . SimpleNode.interface $ node0)
                  )
            disconnectPort (SimpleNode.interface node0) 0
            connectNICs switchNIC (SimpleNode.interface node2)
            connectNICs switchNIC (SimpleNode.interface node0)
            (frame, _) <- runConcurrently $ (,) 
              <$> ( Concurrently . SimpleNode.runOp node0 $ do 
                      SimpleNode.send defaultMessage broadcastAddr
                      SimpleNode.receive
                      
                  )
              <*> ( Concurrently . SimpleNode.runOp node1 $ do
                      void SimpleNode.receive
                      SimpleNode.send defaultMessage (address . SimpleNode.interface $ node0)
                  )
            liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage (payload frame)
          
      ]
