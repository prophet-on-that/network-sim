{-# LANGUAGE OverloadedStrings #-}

module Main where

import NetworkSim.LinkLayer
import qualified NetworkSim.LinkLayer.SimpleNode as SimpleNode

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Control.Monad.STM
import Control.Monad.Reader
import Control.Concurrent.Async

main
  = runTestTT $ TestList
      [ TestLabel "connectNICs" connectNICsT
      , TestLabel "send" sendT
      ]

connectNICsT :: Test
connectNICsT = TestCase $ do
  node0 <- freshMAC >>= atomically . SimpleNode.new
  node1 <- freshMAC >>= atomically . SimpleNode.new
  atomically $ connectNICs (SimpleNode.interface node0) (SimpleNode.interface node1)

sendT :: Test
sendT = TestCase $ do
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
  assertEqual "Transmitted payload does not equal messge" message payload
