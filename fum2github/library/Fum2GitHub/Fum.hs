{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Fum2GitHub.Fum
-- Copyright   :  (C) 2015 Futurice
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
 ----------------------------------------------------------------------------
module Fum2GitHub.Fum (
  -- * Methods
    getAllUsers
  -- * Types
  , AuthToken(..)
  , User(..)
  , UsersResult(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.Aeson.Extra
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Network.HTTP.Client.Fxtra

import           Fum2GitHub.Types (URL(..))

newtype AuthToken = AuthToken { getAuthToken :: String }
    deriving (Eq, Show)

data User = User
  { userName    :: !String
  , userGithub  :: !(Maybe String)
  , userEmail   :: !(Maybe String) -- can be empty?
  }
  deriving (Eq, Show)

instance FromJSON User where
  parseJSON = withObject "User object" $ \v ->
    User <$> v .: "username"
         <*> (emptyToNothing <$> v .: "github")
         <*> v .: "email"

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing x  = Just x

data UsersResult = UsersResult
    { urUsers  :: ![User]
    , urNext   :: !(Maybe URL)
    }
    deriving (Eq, Show)

instance FromJSON UsersResult where
    parseJSON = withObject "FUM users result object" p
        where p v = UsersResult <$> v .: "results"
                                <*> v .: "next"

-- | Get all users from the FUM API.
getAllUsers :: (MonadThrow m, MonadHTTP m, MonadLogger m)
            => AuthToken -- ^ FUM authentication token
            -> URL       -- ^ Initial url
            -> m [User]
getAllUsers token url =
    concat `liftM` getPaginatedResponses (reqBuilder token) resParser url

reqBuilder :: MonadThrow m => AuthToken -> URL -> m Request
reqBuilder token url = do
    baseReq <- parseUrl $ getURL url
    let authHeader = ("Authorization", E.encodeUtf8 . T.pack $ "Token " ++ getAuthToken token)
    return $ baseReq { requestHeaders = authHeader : requestHeaders baseReq }

resParser :: MonadThrow m => Response LBS.ByteString -> m (Maybe URL, [User])
resParser res = do
    UsersResult { urNext = next, urUsers = users } <- decode' $ responseBody res
    return (next, users)
