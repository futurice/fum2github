{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.GitHub (
    getNextUrl,
    getOrgMembers,
    OAuthToken(OAuthToken),
    OrgMember(getOrgMember),
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
import           Fum2GitHub.Util (
    getAPIAll,
    URL(URL, getURL))
import           Network.HTTP.Conduit (
    applyBasicAuth,
    parseUrl,
    Request,
    requestHeaders,
    Response,
    responseBody,
    responseHeaders)
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Text.Regex (mkRegex, matchRegex)


newtype OAuthToken = OAuthToken { getOAuthToken :: String }


-- TODO: return the error message e.g. "Invalid URL" instead of just Nothing.
prepareRequest :: URL -> OAuthToken -> Maybe Request
prepareRequest url token = do
    baseReq <- parseUrl $ getURL url
    let agHdr = ("User-Agent", "https://github.com/futurice/fum2github")
        authReq = applyBasicAuth
                    (E.encodeUtf8 . T.pack . getOAuthToken $ token)
                    "x-oauth-basic" baseReq
    return authReq { requestHeaders = agHdr : requestHeaders authReq }


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

-- Prepare the Request for the next API page of results.
getApiNext :: OAuthToken -> Response LBS.ByteString -> Maybe Request
getApiNext token response = do
    nextUrl <- getNextUrl (responseHeaders response)
    prepareRequest nextUrl token


getAPIResults :: Response LBS.ByteString -> Either String [Aeson.Value]
getAPIResults resp =
    case Aeson.eitherDecode (responseBody resp) :: Either String Aeson.Array of
      Left msg -> Left msg
      Right arrJ -> Right $ Vector.toList arrJ


newtype OrgMember = OrgMember { getOrgMember :: String }

instance Aeson.FromJSON OrgMember where
  parseJSON = Aeson.withObject "Org Member object" p
    where p v = OrgMember <$> v .: "login"


-- Get all members of the organisation.
getOrgMembers :: String -> OAuthToken -> IO (Either String [OrgMember])
getOrgMembers orgName token = do
    let url = URL $ "https://api.github.com/orgs/" ++ orgName ++ "/members"
    case prepareRequest url token of
      Nothing -> return $ Left "error preparing GitHub Org HTTP request"
      Just req -> do
        resultsE <- getAPIAll req (getApiNext token) getAPIResults
        case resultsE of
          Left msg -> return $ Left msg
          Right results ->
            return $ traverse (parseEither Aeson.parseJSON) results
