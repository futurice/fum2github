{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Client.Fxtra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Network.HTTP.Client.Fxtra (
    getPaginatedResponses
  , getSingleResponse
  , parseUrl
  , requestUrl
  , Manager
  , newManager
  , tlsManagerSettings
  , module Control.Monad.HTTP
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Monad.HTTP
import Data.ByteString.Lazy as L
import Data.ByteString.Char8 as BSChar8
import Data.Text as T
import Network.HTTP.Client (parseUrl, Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Fetch paginated resourses.
getPaginatedResponses :: (MonadHTTP m, MonadLogger m)
                      => (a -> m Request)                          -- ^ Request builder
                      -> (Response L.ByteString -> m (Maybe a, b)) -- ^ Response parser into next request data, and parsed response value
                      -> a                                         -- ^ Initial request seed
                      -> m [b]
getPaginatedResponses builder parser = go
    where go s = do req <- builder s
                    $logInfo $ T.append "getPaginatedResponses: " $ T.pack $ requestUrl req
                    res <- httpLbs req
                    (next, v) <- parser res
                    case next of
                        Just next' -> (v :) `liftM` go next'  -- TODO: use fmap with base >=4.8
                        Nothing    -> return [v]

-- | Fancier version of 'httpLbs'.
getSingleResponse :: (MonadHTTP m, MonadLogger m)
                  => (a -> m Request)                          -- ^ Request builder
                  -> (Response L.ByteString -> m (Maybe a, b)) -- ^ Response parser into next request data, and parsed response value
                  -> a                                         -- ^ Initial request seed
                  -> m b
getSingleResponse builder parser s = do
  req <- builder s
  $logInfo $ T.append "getSingleResponse: " $ T.pack $ requestUrl req
  res <- httpLbs req
  snd <$> parser res

-- | Short, one line summary of response. Useful for logging
requestUrl :: Request -> String
requestUrl req = s ++ h ++ p ++ q
  where s = if secure req then "https://" else "http://"
        h = BSChar8.unpack $ host req
        p = BSChar8.unpack $ path req
        q = BSChar8.unpack (queryString req)
