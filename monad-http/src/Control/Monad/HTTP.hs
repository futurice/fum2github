{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.HTTP
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'MonadHTTP' class with basic HTTP functionality.
----------------------------------------------------------------------------
module Control.Monad.HTTP (
  -- * Class
    MonadHTTP(..)
  , BodyReaderM
  -- * Transformer
  , HttpT(..)
  -- * Utilities
  , httpLbs
  , brConsume
  -- * Re-exports
  , Request(..)
  , Response(..)
  ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Data.ByteString as S
import           Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as H
import           Network.HTTP.Client hiding (Manager, withManager, withResponse, httpLbs, brConsume, brRead)

type BodyReaderM m = m S.ByteString

class (Applicative m, Monad m) => MonadHTTP m where
    withResponse :: Request -> (Response (BodyReaderM m) -> m a) -> m a
    brRead :: BodyReaderM m -> m S.ByteString

-- like in https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/src/Control-Monad-Catch.html#instance%20MonadThrow%20(IdentityT%20m)
instance MonadHTTP m => MonadHTTP (IdentityT m) where
    withResponse req f = lift $ withResponse req (runIdentityT . f . fmap lift)
    brRead = lift . brRead . runIdentityT

instance MonadHTTP m => MonadHTTP (ReaderT r m) where
    withResponse req f = ReaderT $ \r -> withResponse req $ \res -> runReaderT (f $ fmap lift res) r
    brRead x = ReaderT $ \r -> brRead (runReaderT x r)

instance MonadHTTP m => MonadHTTP (LoggingT m) where
    withResponse req f = LoggingT $ \r -> withResponse req $ \res -> runLoggingT (f $ fmap lift res) r
    brRead x = LoggingT $ \r -> brRead (runLoggingT x r)

instance MonadHTTP m => MonadHTTP (NoLoggingT m) where
    withResponse req f = lift $ withResponse req (runNoLoggingT . f . fmap lift)
    brRead = lift . brRead . runNoLoggingT

-- | A convenience wrapper around 'withResponse' which reads in the entire response body and immediately releases resources.
httpLbs :: MonadHTTP m => Request -> m (Response L.ByteString)
httpLbs req = withResponse req $ \res -> do
    bss <- brConsume $ responseBody res
    return res { responseBody = L.fromChunks bss }

brConsume :: MonadHTTP m => BodyReaderM m -> m [S.ByteString]
brConsume brRead' =
    go id
  where
    go front = do
        x <- brRead'
        if S.null x
            then return $ front []
            else go (front . (x:))

--
newtype HttpT m a = HttpT { runHttpT :: H.Manager -> m a }

instance Functor m => Functor (HttpT m) where
    fmap f = mapHttpT (fmap f)

instance Applicative m => Applicative (HttpT m) where
    pure    = liftHttpT . pure
    f <*> v = HttpT $ \r -> runHttpT f r <*> runHttpT v r

instance Monad m => Monad (HttpT m) where
    return = liftHttpT . return
    m >>= k  = HttpT $ \r -> do
        a <- runHttpT m r
        runHttpT (k a) r

instance MonadIO m => MonadIO (HttpT m) where
    liftIO = liftHttpT . liftIO

instance MonadThrow m => MonadThrow (HttpT m) where
    throwM = liftHttpT . throwM

instance MonadCatch m => MonadCatch (HttpT m) where
    catch m c = HttpT $ \r -> runHttpT m r `catch` \e -> runHttpT (c e) r

instance MonadMask m => MonadMask (HttpT m) where
    mask a = HttpT $ \r -> mask $ \u -> runHttpT (a $ mapHttpT u) r
    uninterruptibleMask a = HttpT $ \r -> uninterruptibleMask $ \u -> runHttpT (a $ mapHttpT u) r

instance MonadLogger m => MonadLogger (HttpT m) where
    monadLoggerLog a b c d = liftHttpT $ monadLoggerLog a b c d

instance MonadLoggerIO m => MonadLoggerIO (HttpT m) where
    askLoggerIO = liftHttpT askLoggerIO

instance MonadTrans HttpT where
    lift = liftHttpT

mapHttpT :: (m a -> m b) -> HttpT m a -> HttpT m b
mapHttpT f m = HttpT $ f . runHttpT m

liftHttpT :: m a -> HttpT m a
liftHttpT = HttpT . const

-- | TODO: Generalise to MonadIO + MonadMask?
instance m ~ IO => MonadHTTP (HttpT m) where
    withResponse req f = HttpT (\mgr -> H.withResponse req mgr (flip runHttpT mgr . f . fmap liftHttpT))
    brRead br = HttpT $ \mgr ->  H.brRead (runHttpT br mgr)
