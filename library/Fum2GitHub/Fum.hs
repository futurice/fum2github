module Fum2GitHub.Fum (
    getAPIAll,
    User(User), username, github,
    userFromAPI,
) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import           Network.HTTP.Conduit (
    httpLbs,
    parseUrl,
    requestHeaders,
    responseBody,
    withManager)
import qualified Text.JSON as JSON


-- Get the response body from url using authToken.
getHttp :: String -> String -> IO LBS.ByteString
getHttp url authToken = do
    baseReq <- parseUrl url
    let authHdr = (CI.mk . E.encodeUtf8 . T.pack $ "Authorization",
                   E.encodeUtf8 . T.pack $ "Token " ++ authToken)
        req = baseReq { requestHeaders = authHdr : requestHeaders baseReq }
    body <- withManager $ \manager -> do
        resp <- httpLbs req manager
        liftIO . return $ responseBody resp
    return body


-- Get all JSON results from url using authToken, following all pages to end.
getAPIAll :: String -> String -> IO [JSON.JSValue]
getAPIAll url authToken = do
    putStrLn url -- debug logging
    lbsBody <- getHttp url authToken
    let strBody = LT.unpack . LE.decodeUtf8 $ lbsBody
        obj = case JSON.decode strBody
                of JSON.Error err -> error err
                   JSON.Ok (JSON.JSObject obj) -> obj
                   _ -> error $ "API response is not a JSON Object: " ++ strBody
        assocList = JSON.fromJSObject obj
        map = Map.fromList assocList
        results = case Map.lookup "results" map
                    of Nothing -> error "“results” missing in FUM API response"
                       Just r -> case r
                                   of JSON.JSArray rList -> rList
                                      _ -> error ("invalid “results” " ++
                                             JSON.encode r ++ " in FUM API")
        next = case Map.lookup "next" map
                 of Nothing -> error "“next” missing from FUM API response"
                    Just x -> x
    tail <- case next
              of JSON.JSNull -> return []
                 JSON.JSString jss -> (getAPIAll (JSON.fromJSString jss)
                                       authToken)
                 _ -> error $ "Bad “next” in FUM API: " ++ JSON.encode next
    return (results ++ tail)


data User = User { username :: String, github :: String } deriving (Show, Eq)

userFromAPI :: JSON.JSValue -> User
userFromAPI (JSON.JSObject obj) =
    let assocList = JSON.fromJSObject obj
        map = Map.fromList assocList
        username = case Map.lookup "username" map
                     of Just (JSON.JSString jss) -> JSON.fromJSString jss
                        _ -> error ("invalid “username” for FUM API user: " ++
                                    JSON.encode obj)
        github = case Map.lookup "github" map
                   of Just (JSON.JSString jss) -> JSON.fromJSString jss
                      _ -> error ("invalid “github” from FUM API user: " ++
                                  JSON.encode obj)
    in User {username=username, github=github}
userFromAPI x = error ("Invalid FUM API user data: " ++ JSON.encode x)
