{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Caching
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'CachingT' transformer to add caching to 'MonadHTTP' monads.
----------------------------------------------------------------------------
module Control.Monad.Caching (
    CachingT(..)
  , mapCachingT
  , liftCachingT
  , isCacheable
  -- * Cache
  , Cache(..)
  , nullCache
  , responseDirectoryCache
  ) where

import Control.Applicative
import Control.Monad.Caching.ResponseMeta
import Control.Monad.Catch
import Control.Monad.HTTP
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Aeson
import Data.ByteString as S
import Data.ByteString.Lazy as L
import Data.CaseInsensitive
import Data.Digest.Pure.SHA
import Network.HTTP.Types
import System.Directory
import System.FilePath
import System.IO.Fxtra

data Cache m = Cache
    { cacheGet :: Request -> m (Maybe (Response (BodyReaderM m)))
    , cachePut :: Request -> Response (BodyReaderM m) -> m ()
    }

nullCache :: Monad m => Cache m
nullCache = Cache
  { cacheGet = \_   -> return Nothing
  , cachePut = \_ _ -> return ()
  }

diskCachePut :: (MonadIO m, MonadMask m) => FilePath -> Response (BodyReaderM m) -> m ()
diskCachePut p res = diskCachePut' p res `catch` returnOnIOError ()

diskCachePut' :: (MonadIO m, MonadMask m) => FilePath -> Response (BodyReaderM m) -> m ()
diskCachePut' p res = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory p
    liftIO $ L.writeFile metaPath $ encode (toResponseMeta res)
    withFile dataPath WriteMode $ \dataHandle -> do
        bodyloop dataHandle
  where dataPath = p `addExtension` "data"
        metaPath = p `addExtension` "meta"
        readBody = responseBody res
        bodyloop hdl = go
             where go = do chunk <- readBody
                           if S.null chunk
                              then return ()
                              else (liftIO $ S.hPutStr hdl chunk) >> go

readSize :: Int
readSize = 4096

diskCacheGet ::  (MonadIO m, MonadMask m) => FilePath -> m (Maybe (Response (BodyReaderM m)))
diskCacheGet p = diskCacheGet' p `catch` returnOnIOError Nothing

mkBodyReader :: MonadIO m => Handle -> BodyReaderM m
mkBodyReader hdl = liftIO $ hGetSome hdl readSize

diskCacheGet' ::  (MonadIO m, MonadMask m) => FilePath -> m (Maybe (Response (BodyReaderM m)))
diskCacheGet' p = do
    mmeta <- liftIO $ decode' <$> L.readFile metaPath
    case mmeta of
        Nothing -> return Nothing
        -- TODO, this is fishy
        Just meta -> do hdl <- liftIO $ openFile dataPath ReadMode
                        return $ Just $ fromResponseMeta meta (mkBodyReader hdl) (hClose hdl)
    where dataPath = p `addExtension` "data"
          metaPath = p `addExtension` "meta"

-- | Monad transformer that adds a new caching to 'MonadHTTP' function.
newtype CachingT m a = CachingT { runCachingT :: Cache m -> m a }

instance Functor m => Functor (CachingT m) where
    fmap f = mapCachingT (fmap f)

instance Applicative m => Applicative (CachingT m) where
    pure    = liftCachingT . pure
    f <*> v = CachingT $ \r -> runCachingT f r <*> runCachingT v r

instance Monad m => Monad (CachingT m) where
    return = liftCachingT . return
    m >>= k  = CachingT $ \r -> do
        a <- runCachingT m r
        runCachingT (k a) r

instance MonadIO m => MonadIO (CachingT m) where
    liftIO = liftCachingT . liftIO

instance MonadThrow m => MonadThrow (CachingT m) where
    throwM = liftCachingT . throwM

instance MonadCatch m => MonadCatch (CachingT m) where
    catch m c = CachingT $ \r -> runCachingT m r `catch` \e -> runCachingT (c e) r

instance MonadMask m => MonadMask (CachingT m) where
    mask a = CachingT $ \r -> mask $ \u -> runCachingT (a $ mapCachingT u) r
    uninterruptibleMask a = CachingT $ \r -> uninterruptibleMask $ \u -> runCachingT (a $ mapCachingT u) r

instance MonadLogger m => MonadLogger (CachingT m) where
    monadLoggerLog a b c d = liftCachingT $ monadLoggerLog a b c d

instance MonadLoggerIO m => MonadLoggerIO (CachingT m) where
    askLoggerIO = liftCachingT askLoggerIO

instance MonadTrans CachingT where
    lift = liftCachingT

mapCachingT :: (m a -> m b) -> CachingT m a -> CachingT m b
mapCachingT f m = CachingT $ f . runCachingT m

liftCachingT :: m a -> CachingT m a
liftCachingT = CachingT . const

instance (MonadIO m, MonadMask m, MonadHTTP m) => MonadHTTP (CachingT m) where
    withResponse = withResponse0
    brRead x = CachingT $ \r -> brRead (runCachingT x r)

withResponse0 :: (MonadIO m, MonadMask m, MonadHTTP m)
              => Request
              -> (Response (BodyReaderM (CachingT m)) -> CachingT m a)
              -> CachingT m a
withResponse0 req f = CachingT $ \cacheSettings -> withResponse1 cacheSettings req f

withResponse1 :: (MonadIO m, MonadMask m, MonadHTTP m)
              => Cache m
              -> Request
              -> (Response (BodyReaderM (CachingT m)) -> CachingT m a)
              -> m a
withResponse1 cache req f =
    withResponse2 cache req $ \res ->
        runCachingT (f $ fmap liftCachingT res) cache

withResponse2 :: (MonadIO m, MonadMask m, MonadHTTP m)
              => Cache m
              -> Request
              -> (Response (BodyReaderM m) -> m a)
              -> m a
withResponse2 cache req f = do
    maybeRes <- cacheGet cache req
    case maybeRes of
        Just res -> f res
        Nothing  -> do withResponse req $ \res -> cachePut cache req res
                       withResponse2 cache req f


-- Internals

-- | TODO: this doesn't that request body is empty
isCacheable :: Request -> Bool
isCacheable req = methodGet == method req

flattenRequest :: Request -> L.ByteString
flattenRequest req = fromChunks chunks
  where chunks = [ host req
                 , method req
                 , path req
                 , queryString req
                 ] ++ headerChunks
        headerChunks = Prelude.concatMap (\(n, v) -> [foldedCase n, v]) $ requestHeaders req

responseDirectoryCache :: (MonadIO m, MonadMask m)
                       => FilePath -- ^ Root directory of the cache
                       -> Cache m
responseDirectoryCache root = Cache
  { cacheGet = \req -> diskCacheGet (hashPath req)
  , cachePut = \req res -> if isCacheable req
                              then diskCachePut (hashPath req) res
                              else return ()
  }
  where hashPath k = let digest       = showDigest . sha1 . flattenRequest $ k
                         (aa,bb,rest) = splitAt2 2 digest
                     in root </> aa </> bb </> rest

splitAt2 :: Int -> [a] -> ([a], [a], [a])
splitAt2 n l = let (a, b) = Prelude.splitAt n l
                   (c, d) = Prelude.splitAt n b
               in (a, c, d)
