{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.Fum (
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
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.IO (hPutStrLn, stderr)

-- Get the response body from url using authToken.
getHttp :: String -> String -> IO LBS.ByteString
getHttp url authToken = do
    hPutStrLn stderr $ "FUM GET: " ++ url -- debug logging
    baseReq <- parseUrl url
    let authHeader = ("Authorization", E.encodeUtf8 . T.pack $ "Token " ++ authToken)
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
  , urNext :: Maybe String
  }
  deriving (Eq, Show)

instance FromJSON UsersResult where
  parseJSON x = withObject "FUM users result object" p x
    where p v = UsersResult <$> v .: "results"
                            <*> v .: "next"

getSingle :: String -> String -> ExceptT String IO UsersResult
getSingle authToken url = ExceptT $ eitherDecode <$> getHttp url authToken

getAll :: String -> String -> ExceptT String IO [User]
getAll authToken url = do
  UsersResult users next <- getSingle authToken url
  case next of
    Nothing    -> return users
    Just next' -> (users ++) <$> getAll authToken next'

-- Get all users from the FUM API.
getAllUsers :: String -> String -> IO (Either String [User])
getAllUsers usersUrl authToken = runExceptT $ getAll authToken usersUrl
