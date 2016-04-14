module NetworkSim.LinkLayer.Logging
  ( DeviceName
  , PortNum
  , announce
  , record
  , recordWithPort
    -- * Re-exports
  , module NetworkSim.LoggingT  
  ) where

import NetworkSim.LoggingT 
import NetworkSim.LinkLayer.MAC

import qualified Data.Text as T
import Data.Time
import Control.Monad.IO.Class
import Data.Monoid
import System.Log.FastLogger

type DeviceName = T.Text
type PortNum = Int

timeFormat
  = "%Y-%m-%dT%H:%M:%S%QZ"

announce
  :: (MonadIO m, MonadLogger m)
  => T.Text -- ^ Message to log
  -> m ()
announce msg = do
  now <- formatTime defaultTimeLocale timeFormat <$> liftIO getCurrentTime
  let
    logStr
      = "[" <> toLogStr now <> "] " <> toLogStr msg
  logMsg logStr
  
record
  :: (MonadIO m, MonadLogger m)
  => DeviceName
  -> MAC
  -> T.Text -- ^ Message to log
  -> m ()
record deviceName mac msg = do
  now <- formatTime defaultTimeLocale timeFormat <$> liftIO getCurrentTime
  let
    logStr
      = "[" <> toLogStr now <> sep <> toLogStr deviceName <> sep <> (toLogStr . show) mac <> "] " <> toLogStr msg
      where
        sep
          = " * "
  logMsg logStr

recordWithPort
  :: (MonadIO m, MonadLogger m)
  => DeviceName
  -> MAC
  -> PortNum
  -> T.Text -- ^ Message to log
  -> m ()
recordWithPort deviceName mac portNum msg = do
  now <- formatTime defaultTimeLocale timeFormat <$> liftIO getCurrentTime
  let
    logStr
      = "[" <> toLogStr now <> sep <> toLogStr deviceName <> sep <> (toLogStr . show) mac <> "(" <> (toLogStr . show) portNum <> ")" <> "] " <> toLogStr msg
      where
        sep
          = " * "
  logMsg logStr
