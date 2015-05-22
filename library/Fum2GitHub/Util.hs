module Fum2GitHub.Util (
    getAPIAll,
    getHttp,
    URL(URL, getURL),
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Conduit as NHC
import           Network.HTTP.Conduit (
    httpLbs,
    Request,
    Response,
    withManager)


newtype URL = URL { getURL :: String } deriving (Eq, Show)


getHttp :: Request -> IO (Response LBS.ByteString)
getHttp request = do
    print $ NHC.host request -- debug
    withManager $ \manager -> httpLbs request manager


-- Get all API results, following all pages to the end.
getAPIAll :: Request ->
             (Response LBS.ByteString -> Maybe Request) ->
             (Response LBS.ByteString -> Either String [a]) ->
             IO (Either String [a])
getAPIAll request getNextReq getResults = do
    resp <- getHttp request
    case getResults resp of
      Left msg -> return (Left msg)
      Right results -> do
        tailE <- case getNextReq resp of
                   Nothing -> return (Right [])
                   Just nextReq -> getAPIAll nextReq getNextReq getResults
        case tailE of
          Left msg -> return (Left msg)
          Right tail -> return . Right $ results ++ tail
