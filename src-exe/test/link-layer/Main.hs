{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import NetworkSim.LinkLayer
import NetworkSim.LinkLayer.SimpleNode (SimpleNode)
import qualified NetworkSim.LinkLayer.SimpleNode as SimpleNode
import NetworkSim.LinkLayer.Hub (Hub)
import qualified NetworkSim.LinkLayer.Hub as Hub
import NetworkSim.LinkLayer.Switch (Switch)
import qualified NetworkSim.LinkLayer.Switch as Switch

import Test.Tasty 
import Test.Tasty.HUnit hiding (testCase)
import qualified Test.Tasty.HUnit as Tasty
import Control.Monad.Reader
import Control.Concurrent.Async.Lifted
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Foldable
import System.Log.FastLogger
import Data.Time
import Control.Concurrent (threadDelay)
import Data.Word

defaultMessage
  = "Hello, world!"

logFile
  = "link-layer-tests.out"
  
main 
  = bracket (newFileLoggerSet defaultBufSize logFile) rmLoggerSet $ \loggerSet -> do 
      let
        testCase testDesc test
          = Tasty.testCase testDesc $ do 
              printDesc
              runLoggingT loggerSet test
              pushLogStrLn loggerSet ""
          where
            printDesc = do 
              pushLogStrLn loggerSet str
              pushLogStrLn loggerSet (toLogStr testDesc)
              pushLogStrLn loggerSet str
              pushLogStrLn loggerSet ""
              where
                str
                  = toLogStr $ replicate (length testDesc) '#'
                
      defaultMain $ testGroup "Link-layer Tests" 
        [ testGroup "NIC"
            [ testGroup "Connecting"
                [ testCase "Connect NICs" $ do 
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    connectNICs node0 node1

                , testCase "Connect multiple NICs" $ do 
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    node2 <- newNIC 2 False
                    connectNICs node0 node2
                    connectNICs node1 node2

                , testCase "Connecting fails when no free ports available" $ do 
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

                , testCase "Connecting same NIC fails" $ do 
                    node0 <- newNIC 2 False
                    let
                      handler (ConnectToSelf _)
                        = return ()
                      handler e
                        = throwM e
                    handle handler $ do 
                      connectNICs node0 node0
                      liftIO $ assertFailure "No exception thrown"
                ]
            
            , testGroup "Transmitting"
                [ testCase "Send and receive" $ do 
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    connectNICs node0 node1
                    let
                      frame
                        = Frame (address node1) (address node0) defaultMessage
                    (_, payload . snd -> result) <- concurrently
                                     (atomically' $ sendOnNIC frame node0 0)
                                     (atomically' $ receiveOnNIC node1)
                    liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result
                    
                , testCase "Receive and reply" $ do 
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
                        atomically' $ sendOnNIC frame node0 0
                        fmap (payload . snd) . atomically' $ receiveOnNIC node0
                        
                      prog1 = do
                        msg <- fmap (payload . snd) . atomically' $ receiveOnNIC node1
                        let
                          frame
                            = Frame addr0 addr1 msg1
                        atomically' $ sendOnNIC frame node1 0
                        return msg
                    
                    (payload0, payload1) <- concurrently prog0 prog1
                    liftIO $ do
                      assertEqual "Payload 0 does not equal message" msg1 payload0
                      assertEqual "Payload 1 does not equal message" msg0 payload1
                ]

            , testGroup "Broadcast" 
                [ testCase "NIC receives broadcast message" $ do
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    connectNICs node0 node1
                    let
                      frame
                        = Frame broadcastAddr (address node0) defaultMessage
                    atomically' $ sendOnNIC frame node0 0
                    inFrame <- fmap snd . atomically' $ receiveOnNIC node1
                    liftIO $ do
                      assertEqual "Transmitted frame is not identified as a broadcast" Broadcast (destination inFrame)
                      assertEqual "Received payload does not equal message" defaultMessage (payload inFrame)
                ]

            , testGroup "Promiscuous mode" 
                [ testCase "Non-promiscuous NIC ignores frame with other address" $ do
                    addr <- liftIO freshMAC
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    connectNICs node0 node1
                    let
                      frame
                        = Frame addr (address node0) defaultMessage
                      frame'
                        = frame { destination = address node1 } 
                    atomically' $ do
                      sendOnNIC frame node0 0
                      sendOnNIC frame' node0 0 
                    inFrame <- fmap snd . atomically' $ receiveOnNIC node1
                    liftIO $ assertEqual "Expect frame with other destination to be dropped" (Unicast $ address node1) (destination inFrame)
                
                , testCase "Promiscuous NIC accepts frame with other address" $ do
                    addr <- liftIO freshMAC
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 True
                    connectNICs node0 node1
                    let
                      frame
                        = Frame addr (address node0) defaultMessage
                    atomically' $ sendOnNIC frame node0 0
                    void . atomically' $ receiveOnNIC node1
                
                , testCase "Enabling promiscuous mode functions correctly" $ do
                    addr <- liftIO freshMAC
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    setPromiscuity node1 True
                    connectNICs node0 node1
                    let
                      frame
                        = Frame addr (address node0) defaultMessage
                    atomically' $ sendOnNIC frame node0 0
                    void . atomically' $ receiveOnNIC node1
                    
                , testCase "Disabling promiscuous mode functions correctly" $ do
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
                    atomically' $ do
                      sendOnNIC frame node0 0
                      sendOnNIC frame' node0 0 
                    inFrame <- fmap snd . atomically' $ receiveOnNIC node1
                    liftIO $ assertEqual "Expect frame with other destination to be dropped" (Unicast $ address node1) (destination inFrame)
                ]

            , testGroup "Disconnecting" 
                [ testCase "Disconnecting impossible when already disconnected" $ do 
                    node0 <- newNIC 1 False
                    let
                      handler (PortDisconnected _ 0)
                        = return ()
                      handler e
                        = throwM e
                    handle handler $ do
                      disconnectPort node0 0
                      liftIO $ assertFailure "No exception thrown"
                      
                , testCase "Buffer still available after disconnection" $ do 
                    node0 <- newNIC 1 False
                    node1 <- newNIC 1 False
                    connectNICs node0 node1
                    let
                      frame
                        = Frame (address node1) (address node0) defaultMessage
                    atomically' $ sendOnNIC frame node0 0 
                    disconnectPort node1 0
                    result <- fmap (payload . snd) . atomically' $ receiveOnNIC node1
                    liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result
                ]
            ]

        , testGroup "Hub"
            [ testCase "Replicate" $ do
                (hub, [node0, node1]) <- hubStarNetwork 2
                withAsync (Hub.run hub) . const $ do
                  SimpleNode.runOp node0 $ SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                  result <- payload <$> SimpleNode.runOp node1 SimpleNode.receive
                  liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result
                
            , testCase "Hub handles broadcast correctly" $ do 
                (hub, [node0, node1, node2]) <- hubStarNetwork 3
                withAsync (Hub.run hub) . const $ do
                  SimpleNode.runOp node0 $ SimpleNode.send defaultMessage broadcastAddr
                  void $ SimpleNode.runOp node1 SimpleNode.receive
                  void $ SimpleNode.runOp node2 SimpleNode.receive
            
            , testCase "Message replicated several times" $ do
                node0 <- newNIC 1 False 
                node1 <- newNIC 1 False
                hub0 <- Hub.new 2
                hub1 <- Hub.new 2
                connectNICs node0 (Hub.interface hub0)
                connectNICs (Hub.interface hub0) (Hub.interface hub1)
                connectNICs (Hub.interface hub1) node1
                
                withAsync (Hub.run hub0) . const $
                  withAsync (Hub.run hub1) . const $ do 
                    let
                      frame
                        = Frame (address node1) (address node0) defaultMessage
                    atomically' $ sendOnNIC frame node0 0
                    result <- fmap (payload . snd) . atomically' $ receiveOnNIC node1
                    liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result

            , testCase "Hub operates when not fully connected" $ do
                hub <- Hub.new 5
                [node0, node1] <- replicateM 2 SimpleNode.new
                connectNICs (SimpleNode.interface node0) (Hub.interface hub)
                connectNICs (SimpleNode.interface node1) (Hub.interface hub)
                withAsync (Hub.run hub) . const $ do
                  void $ runConcurrently $ (,) 
                    <$> ( Concurrently . SimpleNode.runOp node0 $ do 
                            SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                            void SimpleNode.receive
                            
                        )
                    <*> ( Concurrently . SimpleNode.runOp node1 $ do
                            void SimpleNode.receive
                            SimpleNode.send defaultMessage (address . SimpleNode.interface $ node0)
                        )
            ]

        , testGroup "Switch" 
            [ testCase "Single message forwarded correctly" $ do
                (switch, [node0, node1]) <- switchStarNetwork 2 Nothing
                withAsync (Switch.run switch) . const $ do
                  SimpleNode.runOp node0 $ SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                  result <- payload <$> SimpleNode.runOp node1 SimpleNode.receive
                  liftIO $ assertEqual "Transmitted payload does not equal message" defaultMessage result
            
            , testCase "Broadcast always forwarded" $ do
                 let
                   n
                     = 5
                 (switch, nodes) <- switchStarNetwork n Nothing
                 withAsync (Switch.run switch) . const $ do
                   let
                     prog = do 
                       SimpleNode.send defaultMessage broadcastAddr
                       replicateM_ (fromIntegral n - 1) $ void SimpleNode.receive
                   runConcurrently . sequenceA_ . map (Concurrently . flip SimpleNode.runOp prog) $ nodes
            
            , testCase "Switch learns port of host" $ do
                (switch, [node0, node1, node2]) <- switchStarNetwork 3 Nothing
                setPromiscuity (SimpleNode.interface node2) True
                withAsync (Switch.run switch) . const $ do
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
                    
            , testCase "Switch operates when not fully connected" $ do
                sw <- Switch.new 5 Nothing
                [node0, node1] <- replicateM 2 SimpleNode.new
                connectNICs (SimpleNode.interface node0) (Switch.interface sw)
                connectNICs (SimpleNode.interface node1) (Switch.interface sw)
                withAsync (Switch.run sw) . const $ do
                  void $ runConcurrently $ (,) 
                    <$> ( Concurrently . SimpleNode.runOp node0 $ do 
                            SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                            void SimpleNode.receive
                            
                        )
                    <*> ( Concurrently . SimpleNode.runOp node1 $ do
                            void SimpleNode.receive
                            SimpleNode.send defaultMessage (address . SimpleNode.interface $ node0)
                        )
            
            , testCase "Switch recovers on host reconnect" $ do
                [node0, node1, node2] <- replicateM 3 SimpleNode.new
                switch <- Switch.new 3 Nothing
                let
                  switchNIC
                    = Switch.interface switch
                connectNICs switchNIC (SimpleNode.interface node0)
                connectNICs switchNIC (SimpleNode.interface node1)
                withAsync (Switch.run switch) . const $ do
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

            , testCase "Switch clears database entry after expiry time" $ do
                let
                  ttl
                    = 0.001953125 -- 1/512 seconds.
                (switch, [node0, node1, node2]) <- switchStarNetwork 3 (Just ttl)
                setPromiscuity (SimpleNode.interface node2) True
                withAsync (Switch.run switch) . const $ do
                  void . runConcurrently $ (,,)
                    <$> ( Concurrently . SimpleNode.runOp node0 $
                            SimpleNode.send defaultMessage (address . SimpleNode.interface $ node1)
                        )
                    <*> ( Concurrently . SimpleNode.runOp node1 $
                            void SimpleNode.receive
                        )
                    <*> ( Concurrently . SimpleNode.runOp node2 $
                            void SimpleNode.receive
                        )
                liftIO . threadDelay . truncate $ ttl * 4 * 1000000
                withAsync (Switch.run switch) . const $ do
                  void . runConcurrently $ (,,)
                    <$> ( Concurrently . SimpleNode.runOp node1 $
                            SimpleNode.send defaultMessage (address . SimpleNode.interface $ node0)
                        )
                    <*> ( Concurrently . SimpleNode.runOp node0 $
                            void SimpleNode.receive
                        )
                    <*> ( Concurrently . SimpleNode.runOp node2 $
                            void SimpleNode.receive
                        )
            ]
        ]

---------------
-- Utilities --         
---------------

hubStarNetwork
  :: (MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
  => Word16
  -> m (Hub, [SimpleNode])
hubStarNetwork n = do
  nodes <- replicateM (fromIntegral n) SimpleNode.new
  hub <- Hub.new n
  mapM (connectNICs (Hub.interface hub) . SimpleNode.interface) nodes
  return (hub, nodes)
  
switchStarNetwork
  :: (MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
  => Word16
  -> Maybe NominalDiffTime -- ^ Switch ageing time.
  -> m (Switch, [SimpleNode])
switchStarNetwork n ageingTime = do
  nodes <- replicateM (fromIntegral n) SimpleNode.new
  switch <- Switch.new n ageingTime
  mapM (connectNICs (Switch.interface switch) . SimpleNode.interface) nodes
  return (switch, nodes)

