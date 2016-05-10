module Main where

import NetworkSim.LoggingT
import NetworkSim.LinkLayer
import qualified NetworkSim.LinkLayer.Switch.STP as STP
import qualified NetworkSim.LinkLayer.Hub as Hub

import System.Log.FastLogger
import Control.Monad
import Control.Concurrent.Async.Lifted
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

main = do
  loggerSet <- newStdoutLoggerSet defaultBufSize
  runLoggingT loggerSet star3

switchLoop :: LoggingT IO ()
switchLoop = do 
  [sw0, sw1, sw2, sw3, sw4] <- replicateM 5 $ STP.new 10 STP.defaultPriority
  connectNICs (STP.interface sw0) (STP.interface sw1)
  connectNICs (STP.interface sw1) (STP.interface sw2)
  connectNICs (STP.interface sw2) (STP.interface sw3)
  connectNICs (STP.interface sw3) (STP.interface sw4)
  connectNICs (STP.interface sw4) (STP.interface sw0)
  withAsync (STP.run sw0) . const $ do
    withAsync (STP.run sw1) . const $ do
      withAsync (STP.run sw2) . const $ do
        withAsync (STP.run sw3) . const $ do
          withAsync (STP.run sw4) . const $ do
            liftIO . threadDelay $ 5 * 1000000

star3 :: LoggingT IO ()
star3 = do
  [sw0, sw1, sw2] <- replicateM 3 $ STP.new 10 STP.defaultPriority
  hub <- Hub.new 5
  connectNICs (STP.interface sw0) (Hub.interface hub)
  connectNICs (STP.interface sw1) (Hub.interface hub)
  connectNICs (STP.interface sw2) (Hub.interface hub)
  withAsync (Hub.run hub) . const $ do
    withAsync (STP.run sw0) . const $ do
      withAsync (STP.run sw1) . const $ do
        withAsync (STP.run sw2) . const $ do
          liftIO . threadDelay $ 5 * 1000000
