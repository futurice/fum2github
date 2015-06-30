{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Github.Extra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Github.Fxtra (
    membersOf
  , teamsOf
  , membersOfTeam
  , userInfoFor
  -- * Utilities
  , nextUrl
  , getMulti
  , getSingle
  -- * Aliases
  , URLPart
  , buildUrl
  -- * Teams
  , GithubTeam(..)

  -- * Re-exports
  , module Github.Auth
  , module Github.Data
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Monad.HTTP
import Control.Monad.Logger
import Data.Aeson.Fxtra
import Data.ByteString.Char8 as BSChar8
import Data.ByteString.Lazy as LBS
import Data.CaseInsensitive as CI
import Data.Data
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as E
import GHC.Generics
import Github.Auth
import Github.Data
import Network.HTTP.Client (applyBasicAuth)
import Network.HTTP.Client.Fxtra
import Network.HTTP.Types
import Text.Regex (mkRegex, matchRegex)

-- Do not export!
type URL = String

type URLPart = String

applyGithubAuth :: Maybe GithubAuth -> Request -> Request
applyGithubAuth Nothing req = req
applyGithubAuth (Just (GithubBasicAuth user pass)) req = applyBasicAuth user pass req
applyGithubAuth (Just (GithubOAuth token)) req =
  let header = (mk (BSChar8.pack "Authorization"),  BSChar8.pack ("token " ++ token))
  in req { requestHeaders = header : requestHeaders req }

reqBuilder :: MonadThrow m => Maybe GithubAuth -> URL -> m Request
reqBuilder auth url = do
  req <- parseUrl url
  let req' = applyGithubAuth auth req
  let userAgentHeader = ("User-Agent", "https://github.com/futurice/fum2github")
  let req'' = req'  { requestHeaders = userAgentHeader : requestHeaders req' }
  return req''

resParser :: (MonadThrow m, FromJSON a) => Response LBS.ByteString -> m (Maybe URL, a)
resParser res = do
  let body = responseBody res
      hdrs = responseHeaders res
  value <- throwDecode body
  return (nextUrl hdrs, value)

getMulti :: (MonadThrow m, MonadHTTP m, MonadLogger m, FromJSON a)
          => [URLPart]
          -> Maybe GithubAuth
          -> m [a]
getMulti parts auth = withManager tlsManagerSettings $ \manager -> do
  Prelude.concat `liftM` getPaginatedResponses manager (reqBuilder auth) resParser url
  where url = buildUrl parts

getSingle :: (MonadThrow m, MonadHTTP m, MonadLogger m, FromJSON a)
          => [URLPart]
          -> Maybe GithubAuth
          -> m a
getSingle parts auth = withManager tlsManagerSettings $ \manager -> do
  getSingleResponse manager (reqBuilder auth) resParser url
  where url = buildUrl parts

-- API calls

membersOf :: (MonadThrow m, MonadHTTP m, MonadLogger m) => Maybe GithubAuth -> String -> m [GithubOwner]
membersOf auth organization = getMulti ["orgs", organization, "members"] auth

-- | The information for a single user, by login name.
userInfoFor :: (MonadThrow m, MonadHTTP m, MonadLogger m) => Maybe GithubAuth -> String -> m DetailedOwner
userInfoFor auth userName = getSingle ["users", userName] auth

data GithubTeam = GithubTeam
  { githubTeamId :: Int
  , githubTeamName :: String
  , githubTeamDescription :: Maybe String
  }
  deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData GithubTeam

instance FromJSON GithubTeam where
  parseJSON = withObject "GithubTeam" $ \v ->
    GithubTeam <$> v .: "id"
               <*> v .: "name"
               <*> v .:? "description"

-- | All the users who are members of the specified organization.
--
-- > teamsOf auth "futurice"
--
-- https://developer.github.com/v3/orgs/teams/
teamsOf :: (MonadThrow m, MonadHTTP m, MonadLogger m) => Maybe GithubAuth -> String -> m [GithubTeam]
teamsOf auth organization = getMulti ["orgs", organization, "teams"] auth

-- | All the users who are members of specified team
--
-- > membersOfTeam auth 1337
--
-- https://developer.github.com/v3/orgs/teams/
membersOfTeam :: (MonadThrow m, MonadHTTP m, MonadLogger m) => Maybe GithubAuth -> Int -> m [GithubOwner]
membersOfTeam auth teamId = getMulti ["teams", show teamId, "members"] auth

--

-- | Get the 'URL' with @rel="next"@ from the Link response header.
--
-- The GitHub header looks like this:
-- @
-- Link: <https://some-url>; rel="next", <https://some-url>; rel="last"
-- @
-- We allow multiple Link headers, each with one or more comma-separated values
-- and return the first matching value.
nextUrl :: ResponseHeaders -> Maybe URL
-- TODO rewrite using regex-applicative
nextUrl headers =
    L.foldr foldFunc Nothing matches
    where
      linkHeaders = L.filter ((== "Link") . fst) headers
      strVals = L.map (T.unpack . E.decodeUtf8 . snd) linkHeaders
      re = mkRegex "<([^>]+)>; rel=\"next\""
      matches = L.map (matchRegex re) strVals

      foldFunc :: Maybe [String] -> Maybe URL -> Maybe URL
      foldFunc (Just [x]) _ = Just x
      foldFunc _ x = x

buildUrl :: [URLPart] -> URL
buildUrl parts = "https://api.github.com/" ++ L.intercalate "/" parts
