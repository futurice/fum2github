{-# LANGUAGE DeriveDataTypeable #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Fxtra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Additional stuff to deal with JSON
----------------------------------------------------------------------------
module Data.Aeson.Fxtra (
  -- * Decoding in MonadThrow
    throwDecode
  , throwDecode'
  , JSONError(..)
  -- * Re-export
  , module Data.Aeson
  ) where

import Control.Exception
import Control.Monad.Catch.Fxtra
import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy
import Data.Typeable

newtype JSONError = JSONError String
    deriving (Show, Typeable)

instance Exception JSONError

-- | Like 'decode' but throws an error message in 'JSONError' when decoding fails.
throwDecode :: (MonadThrow m, FromJSON a) => ByteString -> m a
throwDecode = hoistEither . first JSONError . eitherDecode

throwDecode' :: (MonadThrow m, FromJSON a) => ByteString -> m a
throwDecode' = hoistEither . first JSONError . eitherDecode'
