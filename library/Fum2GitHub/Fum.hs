{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.Fum (
    getAllUsers,
    User(User), username, github,
    userFromAPI,
) where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Traversable (traverse)
import qualified Data.Vector as Vector
import           Network.HTTP.Conduit (
    httpLbs,
    parseUrl,
    requestHeaders,
    responseBody,
    withManager)


-- Get the response body from url using authToken.
getHttp :: String -> String -> IO LBS.ByteString
getHttp url authToken = do
    baseReq <- parseUrl url
    let authHdr = ("Authorization", E.encodeUtf8 . T.pack $ "Token " ++ authToken)
        req = baseReq { requestHeaders = authHdr : requestHeaders baseReq }
    body <- withManager $ \manager -> do
        resp <- httpLbs req manager
        liftIO . return $ responseBody resp
    return body


-- Get all JSON results from url using authToken, following all pages to end.
getAPIAll :: String -> String -> IO (Either String [Aeson.Value])
getAPIAll url authToken = do
    putStrLn url -- debug logging
    lbsBody <- getHttp url authToken
    case Aeson.eitherDecode lbsBody :: Either String Aeson.Object of
      Left msg -> return (Left msg)
      Right map -> do
        let resultsE = case HMS.lookup (T.pack "results") map of
                         Nothing -> Left "“results” missing in FUM response"
                         Just v -> case v of
                                     Aeson.Array a -> Right (Vector.toList a)
                                     _ -> Left "FUM “results” is not an array"
        case resultsE of
          Left msg -> return resultsE
          Right results -> do
            let nextE = case HMS.lookup (T.pack "next") map of
                      Nothing -> Left "“next” missing in FUM response"
                      Just v -> case v of
                                  Aeson.String txt -> Right (T.unpack txt)
                                  Aeson.Null -> Right ""
                                  _ -> Left "FUM “next” is not a string"
            case nextE of
              Left msg -> return (Left msg)
              Right next -> do
                tailE <- if null next then
                    return (Right [])
                else
                    getAPIAll next authToken
                case tailE of
                  Left msg -> return (Left msg)
                  Right tail -> return (Right (results ++ tail))


data User = User { username :: String, github :: String } deriving (Show, Eq)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User object" p
    where p v = User <$> v .: "username"
                     <*> v .: "github"

userFromAPI :: Aeson.Value -> Either String User
userFromAPI = parseEither Aeson.parseJSON


-- Get all users from the FUM API.
getAllUsers :: String -> String -> IO (Either String [User])
getAllUsers usersUrl authToken = do
    resultsE <- getAPIAll usersUrl authToken
    return $ case resultsE of
      Left msg -> Left msg
      Right results -> traverse userFromAPI results
