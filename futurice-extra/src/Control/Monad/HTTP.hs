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
  -- * Utilities
  , httpLbs
  , brConsume
  -- * Re-exports
  , Request(..)
  , Response(..)
  ) where

import           Control.Applicative
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
    type ManagerM m :: *
    withManager :: ManagerSettings -> (ManagerM m -> m a) -> m a
    withResponse :: Request -> ManagerM m -> (Response (BodyReaderM m) -> m a) -> m a
    brRead :: BodyReaderM m -> m S.ByteString

instance MonadHTTP IO where
    type ManagerM IO = H.Manager
    withManager = H.withManager
    withResponse = H.withResponse
    brRead = H.brRead

-- like in https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/src/Control-Monad-Catch.html#instance%20MonadThrow%20(IdentityT%20m)
instance MonadHTTP m => MonadHTTP (IdentityT m) where
    type ManagerM (IdentityT m) = ManagerM m
    withManager settings f = lift $ withManager settings (runIdentityT . f)
    withResponse req mgr f = lift $ withResponse req mgr (runIdentityT . f . fmap lift)
    brRead = lift . brRead . runIdentityT

instance MonadHTTP m => MonadHTTP (ReaderT r m) where
    type ManagerM (ReaderT r m) = ManagerM m
    withManager settings f = ReaderT $ \r -> withManager settings $ \mgr -> runReaderT (f mgr) r
    withResponse req mgr f = ReaderT $ \r -> withResponse req mgr $ \res -> runReaderT (f $ fmap lift res) r
    brRead x = ReaderT $ \r -> brRead (runReaderT x r)

instance MonadHTTP m => MonadHTTP (LoggingT m) where
    type ManagerM (LoggingT m) = ManagerM m
    withManager settings f = LoggingT $ \r -> withManager settings $ \mgr -> runLoggingT (f mgr) r
    withResponse req mgr f = LoggingT $ \r -> withResponse req mgr $ \res -> runLoggingT (f $ fmap lift res) r
    brRead x = LoggingT $ \r -> brRead (runLoggingT x r)

instance MonadHTTP m => MonadHTTP (NoLoggingT m) where
    type ManagerM (NoLoggingT m) = ManagerM m
    withManager settings f = lift $ withManager settings (runNoLoggingT . f)
    withResponse req mgr f = lift $ withResponse req mgr (runNoLoggingT . f . fmap lift)
    brRead = lift . brRead . runNoLoggingT

httpLbs :: MonadHTTP m => Request -> ManagerM m -> m (Response L.ByteString)
httpLbs req man = withResponse req man $ \res -> do
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
