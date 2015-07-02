{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Base64.Type
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'ByteString64' is a strict 'ByteString' serialised with base64 encoding.
 ----------------------------------------------------------------------------
module Data.ByteString.Base64.Type (ByteString64(..), module Data.ByteString) where

import Control.Applicative
import Data.Aeson
import Data.ByteString
import Data.ByteString.Base64 as Base64
import Data.Data
import Data.Serialize
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error

-- | Aeson serialisable bytestring. Uses base64 encoding.
newtype ByteString64 = ByteString64 { getByteString64 :: ByteString }
  deriving (Eq, Show, Ord, Data, Typeable)

instance ToJSON ByteString64 where
  toJSON = toJSON . decodeUtf8With ignore  . Base64.encode . getByteString64

instance FromJSON ByteString64 where
  parseJSON v = ByteString64 <$> withText "ByteString" (pure . decodeLenient .  encodeUtf8) v

-- | 'ByteString64' is serialised as 'ByteString'
instance Serialize ByteString64 where
  put = put . getByteString64
  get = ByteString64 <$> get
