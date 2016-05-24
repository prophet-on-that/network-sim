-- | Similar functionality to the
-- <http://hackage.haskell.org/package/monad-logger monad-logger>
-- package, but given more generally, providing a more flexible
-- logging API.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module NetworkSim.LoggingT
  ( MonadLogger (..)
  , LoggingT ()
  , runLoggingT
  , NoLoggingT (..)
  ) where

import System.Log.FastLogger
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Catch

class MonadLogger m where
  logMsg :: ToLogStr msg => msg -> m ()

instance (Monad m, MonadLogger m) => MonadLogger (ReaderT r m) where
  logMsg
    = lift . logMsg

newtype LoggingT m a = LoggingT
  { unLoggingT :: (ReaderT LoggerSet m a)
  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch)

instance MonadTransControl LoggingT where
  type StT LoggingT a = StT (ReaderT LoggerSet) a
  liftWith = defaultLiftWith LoggingT unLoggingT
  restoreT = defaultRestoreT LoggingT

instance MonadBaseControl b m => MonadBaseControl b (LoggingT m) where
  type StM (LoggingT m) a = ComposeSt LoggingT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadIO m => MonadLogger (LoggingT m) where
  logMsg msg
    = LoggingT $ do
        loggerSet <- ask
        liftIO $ pushLogStrLn loggerSet (toLogStr msg)

runLoggingT
  :: LoggerSet
  -> LoggingT m a
  -> m a
runLoggingT loggerSet (LoggingT action)
  = runReaderT action loggerSet
    
newtype NoLoggingT m a = NoLoggingT
  { runNoLoggingT :: m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadThrow, MonadCatch)

instance MonadTrans NoLoggingT where
  lift = NoLoggingT

instance MonadTransControl NoLoggingT where
  type StT NoLoggingT a = a
  liftWith f = NoLoggingT $ f runNoLoggingT
  restoreT = NoLoggingT

instance MonadBaseControl b m => MonadBaseControl b (NoLoggingT m) where
  type StM (NoLoggingT m) a = StM m a
  liftBaseWith f
    = NoLoggingT $
        liftBaseWith $ \runInBase ->
          f $ runInBase . runNoLoggingT
  restoreM = NoLoggingT . restoreM

instance Monad m => MonadLogger (NoLoggingT m) where
  logMsg _
    = return ()
