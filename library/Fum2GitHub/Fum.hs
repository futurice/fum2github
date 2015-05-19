{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.Fum (
    AuthToken(AuthToken),
    getAllUsers,
    User(User), username, github,
    userFromAPI,
) where

import           Control.Applicative ((<*>), (<$>))
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Traversable (traverse)
import qualified Data.Vector as Vector
import           Fum2GitHub.Util (
    getAPIAll,
    URL(URL, getURL))
import           Network.HTTP.Conduit (
    parseUrl,
    Request,
    requestHeaders,
    Response,
    responseBody)


newtype AuthToken = AuthToken { getAuthToken :: String }


-- TODO: return the error message e.g. "Invalid URL" instead of just Nothing.
prepareRequest :: URL -> AuthToken -> Maybe Request
prepareRequest url token = do
    baseReq <- parseUrl $ getURL url
    let tokStr = getAuthToken token
        authHdr = ("Authorization", E.encodeUtf8 . T.pack $ "Token " ++ tokStr)
    return baseReq { requestHeaders = authHdr : requestHeaders baseReq }


getAPIResults :: Response LBS.ByteString -> Either String [Aeson.Value]
getAPIResults resp =
    case Aeson.eitherDecode (responseBody resp) :: Either String Aeson.Object of
      Left msg -> Left msg
      Right map ->
        case HMS.lookup (T.pack "results") map of
          Nothing -> Left "“results” missing in FUM response"
          Just v -> case v of
                      Aeson.Array a -> Right (Vector.toList a)
                      _ -> Left "FUM “results” is not an array"


-- TODO: differentiate between "there was an error" and "there is no next URL"
getAPINext :: AuthToken -> Response LBS.ByteString -> Maybe Request
getAPINext token resp =
    case Aeson.eitherDecode (responseBody resp) :: Either String Aeson.Object of
      Left msg -> Nothing
      Right map ->
        case HMS.lookup (T.pack "next") map of
          Nothing -> Nothing -- error: "“next” missing in FUM response"
          Just v -> case v of
            Aeson.String txt ->
              case prepareRequest (URL . T.unpack $ txt) token of
                Nothing -> Nothing -- error (e.g. Invalid URL format)
                Just req -> Just req
            Aeson.Null -> Nothing -- ok: we've reached the end
            _ -> Nothing -- error: "FUM “next” is not a string"


data User = User { username :: String, github :: String } deriving (Show, Eq)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User object" p
    where p v = User <$> v .: "username"
                     <*> v .: "github"

userFromAPI :: Aeson.Value -> Either String User
userFromAPI = parseEither Aeson.parseJSON


-- Get all users from the FUM API.
getAllUsers :: URL -> AuthToken -> IO (Either String [User])
getAllUsers usersUrl token = do
    case prepareRequest usersUrl token of
      Nothing -> return $ Left "error preparing FUM HTTP request"
      Just req -> do
        resultsE <- getAPIAll req (getAPINext token) getAPIResults
        return $ case resultsE of
          Left msg -> Left msg
          Right results -> traverse userFromAPI results
