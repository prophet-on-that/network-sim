-- | Similar functionality to the
-- <http://hackage.haskell.org/package/monad-logger monad-logger>
-- package, but given more generally, providing a more flexible
-- logging API.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NetworkSim.LoggingT
  ( MonadLogger (..)
  , LoggingT ()
  , runLoggingTFile
  ) where

import System.Log.FastLogger
import Control.Monad.Reader
import Control.Monad.Catch

class MonadLogger m where
  log :: ToLogStr msg => msg -> m ()

newtype LoggingT m a = LoggingT
  { runLoggingT :: ReaderT LoggerSet m a
  } deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadLogger (LoggingT m) where
  log msg
    = LoggingT $ do
        loggerSet <- ask
        liftIO $ pushLogStrLn loggerSet (toLogStr msg)

runLoggingTFile
  :: (MonadIO m, MonadMask m)
  => FilePath
  -> LoggingT m a
  -> m a
runLoggingTFile file action
  = bracket acquire release run 
  where
    acquire
      = liftIO $ newFileLoggerSet defaultBufSize file
    run 
      = runReaderT (runLoggingT action)
    release
      = liftIO . rmLoggerSet
