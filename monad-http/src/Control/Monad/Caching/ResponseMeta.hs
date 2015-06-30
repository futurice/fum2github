{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Caching.ResponseMeta
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'Response' metadata, which we can serialise as JSON
--
 ----------------------------------------------------------------------------
module Control.Monad.Caching.ResponseMeta (ResponseMeta(..), toResponseMeta, fromResponseMeta) where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson.Fxtra
import Data.ByteString as S
import Data.ByteString.Base64.Type
import Data.CaseInsensitive as CI
import Data.Data
import GHC.Generics
import Data.Serialize
import Network.HTTP.Client.Internal
import Network.HTTP.Types
import Test.QuickCheck

-- | 'Response' meta data fields.
data ResponseMeta = ResponseMeta
    { rmStatus  :: !Status
    , rmVersion :: !HttpVersion
    , rmHeaders :: !ResponseHeaders
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData ResponseMeta where
    rnf (ResponseMeta (Status c m) (HttpVersion major minor) hdrs) =
        rnf c `seq`
        rnf m `seq`
        rnf major `seq`
        rnf minor `seq`
        rnf hdrs

toResponseMeta :: Response body -> ResponseMeta
toResponseMeta res = ResponseMeta
    { rmStatus = responseStatus res
    , rmVersion = responseVersion res
    , rmHeaders = responseHeaders res
    }

fromResponseMeta :: ResponseMeta
                 -> body       -- ^ Body
                 -> IO ()      -- ^ Close action
                 -> Response body
fromResponseMeta meta body close = Response
    { responseStatus     = rmStatus meta
    , responseVersion    = rmVersion meta
    , responseHeaders    = rmHeaders meta
    , responseBody       = body
    , responseCookieJar  = createCookieJar []
    , responseClose'     = ResponseClose close
    }

newtype WrappedStatus = WrappedStatus { unwrapStatus :: Status }
newtype WrappedHttpVersion = WrappedHttpVersion { unwrapVersion :: HttpVersion }
newtype WrappedHeader = WrappedHeader { unwrapHeader :: Header }

instance ToJSON WrappedStatus where
    toJSON (WrappedStatus (Status c m)) = toJSON (c, ByteString64 m)

instance ToJSON WrappedHttpVersion where
    toJSON (WrappedHttpVersion (HttpVersion major minor)) = toJSON (minor, major)

instance ToJSON WrappedHeader where
    toJSON (WrappedHeader (n, v)) = toJSON (ByteString64 . CI.original $ n, ByteString64 v )

instance ToJSON ResponseMeta where
    toJSON (ResponseMeta s v h) = object [ "status"  .= WrappedStatus s
                                         , "version" .= WrappedHttpVersion v
                                         , "headers" .= fmap WrappedHeader h
                                         ]

instance FromJSON WrappedStatus where
    parseJSON x = f <$> parseJSON x
        where f (c, m) = WrappedStatus (Status c (getByteString64 m))

instance FromJSON WrappedHttpVersion where
    parseJSON x = f <$> parseJSON x
        where f (major, minor) = WrappedHttpVersion (HttpVersion minor major)

instance FromJSON WrappedHeader where
    parseJSON x = f <$> parseJSON x
        where f (n, v) = WrappedHeader (CI.mk . getByteString64 $ n, getByteString64 v)

instance FromJSON ResponseMeta where
    parseJSON = withObject "ResponseMeta" $ \v ->
        ResponseMeta <$> (unwrapStatus <$> v .: "status")
                     <*> (unwrapVersion <$> v .: "version")
                     <*> (fmap unwrapHeader <$>  v .: "headers")

-- QuickCheck

newtype SByteString = SByteString { getSByteString :: ByteString }

instance Arbitrary SByteString where
    arbitrary = SByteString . S.pack <$> arbitrary
    shrink    = Prelude.map (SByteString . S.pack) . shrink . (S.unpack . getSByteString)

instance Serialize SByteString where
    put = put . getSByteString
    get = SByteString <$> get

type ResponseMetaTuple = ((Int, SByteString), (Int, Int), [(SByteString, SByteString)])

toTuple :: ResponseMeta -> ResponseMetaTuple
toTuple (ResponseMeta (Status code msg) (HttpVersion minor major) headers) =
    ((code, SByteString msg), (major, minor), Prelude.map f headers)
    where f (n, v) = (SByteString $ CI.original n, SByteString v)

fromTuple :: ResponseMetaTuple -> ResponseMeta
fromTuple ((code, msg), (major, minor), headers) =
    (ResponseMeta (Status code $ getSByteString msg) (HttpVersion minor major) (Prelude.map f headers))
    where f (n, v) = (CI.mk $ getSByteString n, getSByteString v)

instance Arbitrary ResponseMeta where
    arbitrary = fromTuple <$> arbitrary
    shrink    = Prelude.map fromTuple . shrink . toTuple

instance Serialize ResponseMeta where
    put = put . toTuple
    get = fromTuple <$> get
