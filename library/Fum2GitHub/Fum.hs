{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.Fum (
    AuthToken(..),
    getAllUsers,
    User(..),
    UsersResult(..)
) where

import           Control.Applicative
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Fum2GitHub.Types(
    URL(URL, getURL))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.IO (hPutStrLn, stderr)

newtype AuthToken = AuthToken { getAuthToken :: String }

-- Get the response body from url using authToken.
getHttp :: URL -> AuthToken -> IO LBS.ByteString
getHttp url token = do
    hPutStrLn stderr $ "FUM GET: " ++ getURL url -- debug logging
    baseReq <- parseUrl $ getURL url
    let authHeader = ("Authorization", E.encodeUtf8 . T.pack $ "Token " ++ getAuthToken token)
        req = baseReq { requestHeaders = authHeader : requestHeaders baseReq }
    withManager tlsManagerSettings $ \manager -> do
      responseBody <$> httpLbs req manager

data User = User
  { userName :: String
  , userGithub :: Maybe String
  , userEmail :: Maybe String -- can be empty?
  }
  deriving (Eq, Show)

instance FromJSON User where
  parseJSON = withObject "User object" p
   where p v = User <$> v .: "username"
                    <*> (emptyToNothing <$> v .: "github")
                    <*> v .: "email"

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing x  = Just x

data UsersResult = UsersResult
  { urUsers :: [User]
  , urNext :: Maybe URL
  }
  deriving (Eq, Show)

instance FromJSON UsersResult where
  parseJSON x = withObject "FUM users result object" p x
    where p v = UsersResult <$> v .: "results"
                            <*> v .: "next"

getSingle :: AuthToken -> URL -> ExceptT String IO UsersResult
getSingle token url = ExceptT $ eitherDecode <$> getHttp url token

getAll :: AuthToken -> URL -> ExceptT String IO [User]
getAll token url = do
  UsersResult users next <- getSingle token url
  case next of
    Nothing    -> return users
    Just next' -> (users ++) <$> getAll token next'

-- Get all users from the FUM API.
getAllUsers :: URL -> AuthToken -> IO (Either String [User])
getAllUsers usersUrl token = runExceptT $ getAll token usersUrl
