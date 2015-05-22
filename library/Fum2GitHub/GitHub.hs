{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.GitHub (
    getNextUrl,
    getOrgMembers,
    OAuthToken(OAuthToken),
    OrgMember(getOrgMember),
    URL(URL),
) where

import           Control.Applicative ((<$>))
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Traversable (traverse)
import qualified Data.Vector as Vector
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Text.Regex (mkRegex, matchRegex)

newtype URL = URL { getURL :: String } deriving (Eq, Show)

newtype OAuthToken = OAuthToken { getOAuthToken :: String } deriving (Eq, Show)

-- Get the response body and headers from url using oAuthToken.
getHttp :: URL -> OAuthToken -> IO (LBS.ByteString, ResponseHeaders)
getHttp url oAuthToken = do
    putStrLn $ "GitHub GET: " ++ getURL url -- debug logging
    baseReq <- parseUrl $ getURL url
    let userAgentHeader = ("User-Agent", "https://github.com/futurice/fum2github")
        authReq = applyBasicAuth
                    (E.encodeUtf8 . T.pack . getOAuthToken $ oAuthToken)
                    "x-oauth-basic" baseReq
        req = authReq { requestHeaders = userAgentHeader : requestHeaders authReq }
    withManager tlsManagerSettings $ \manager -> do
        resp <- httpLbs req manager
        return (responseBody resp, responseHeaders resp)


{-
Get the URL with rel="next" from the Link response header.

The GitHub header looks like this:
Link: <https://some-url>; rel="next", <https://some-url>; rel="last"
We allow multiple ‘Link’ headers, each with one or more comma-separated values
and return the first matching value.
-}
getNextUrl :: ResponseHeaders -> Maybe URL
getNextUrl headers =
    foldr foldFunc Nothing matches
    where
      linkHeaders = filter ((== "Link") . fst) headers
      strVals = map (T.unpack . E.decodeUtf8 . snd) linkHeaders
      re = mkRegex "<([^>]+)>; rel=\"next\""
      matches = map (matchRegex re) strVals

      foldFunc :: Maybe [String] -> Maybe URL -> Maybe URL
      foldFunc (Just [x]) _ = Just (URL x)
      foldFunc _ x = x


-- Get all JSON results from url using oAuthToken, following all pages to end.
getAll :: URL -> OAuthToken -> IO (Either String [Aeson.Value])
getAll url oAuthToken = do
    (body, hdrs) <- getHttp url oAuthToken
    case Aeson.eitherDecode body :: Either String Aeson.Array of
      Left msg -> return $ Left msg
      Right arrJ -> do
        print arrJ
        let arr = Vector.toList arrJ
        case getNextUrl hdrs of
          Nothing -> return $ Right arr
          Just nextUrl -> do
            tailE <- getAll nextUrl oAuthToken
            case tailE of
              Left msg -> return $ Left msg
              Right tail -> return $ Right (arr ++ tail)


newtype OrgMember = OrgMember { getOrgMember :: String }

instance Aeson.FromJSON OrgMember where
  parseJSON = Aeson.withObject "Org Member object" p
    where p v = OrgMember <$> v .: "login"


-- Get all members of the organisation.
getOrgMembers :: String -> OAuthToken -> IO (Either String [OrgMember])
getOrgMembers orgName oAuthToken = do
    let url = URL $ "https://api.github.com/orgs/" ++ orgName ++ "/members"
    resultsE <- getAll url oAuthToken
    case resultsE of
      Left msg -> return $ Left msg
      Right results -> do
        return $ traverse (parseEither Aeson.parseJSON) results
