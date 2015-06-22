{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import           Control.DeepSeq
import           Control.Lens hiding (argument)
import           Control.Monad
import           Control.Monad.Caching
import           Control.Monad.Catch.Fxtra
import           Control.Monad.HTTP
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.List as L
import           Data.Swarm
import qualified Fum2GitHub.Compare as Compare
import qualified Fum2GitHub.Fum as Fum
import Data.Text as T
import           Fum2GitHub.Types
import           Options.Applicative.Fxtra

import           Github.Extra as Github

-- fumApiUsersUrl fumAuthToken githubOrganisation githubOAuthtoken
data Opts = Opts
    { _optsLogging       :: !Bool
    , _optsCaching       :: !Bool
    , _optsCacheDir      :: FilePath
    , _team              :: Maybe String
    , _fumApiUsersUrl    :: !URL
    , _fumAuthToken      :: !Fum.AuthToken
    , _githubOrg         :: !String
    , _githubAuth        :: !GithubAuth
    }
    deriving (Eq, Show)

makeLenses ''Opts

defaultCacheDir :: FilePath
defaultCacheDir = ".cache"

optsParser :: Parser Opts
optsParser = p `withTransformations` ts
  where p = Opts False False <$> strOption (long "cache-dir" <> metavar "CACHEDIR" <> value defaultCacheDir <> showDefault <> hidden <> help "Cache directory")
                             <*> optional (strOption (long "team" <> metavar "TEAM" <> hidden <> help "Use team of organisation"))
                             <*> (URL <$> argument str (metavar "fum-api-users-url"))
                             <*> (Fum.AuthToken <$> argument str (metavar "fum-token"))
                             <*> argument str (metavar "github-organisation")
                             <*> (GithubOAuth <$> argument str (metavar "github-token"))
        ts = [ (set optsLogging False, short 'q' <> long "quiet  "  <> help "Disable logging")
             , (set optsLogging True,  short 'v' <> long "verbose"  <> help "Enable logging")
             , (set optsCaching False,              long "no-cache" <> help "Disable caching")
             , (set optsCaching True,               long "cache"    <> help "Enable caching")
             ]

main :: IO ()
main = execParser optsInfo >>= main'
    where optsInfo = info (helper <*> optsParser) (fullDesc <> header "Print users in GitHub organisation, which aren't in FUM")

getGithubUsers :: (MonadHTTP m, MonadLogger m, MonadThrow m) => Opts -> m [GithubOwner]
getGithubUsers opts =
  case view team opts of
    Just teamName -> do teams <- Github.teamsOf auth (view githubOrg opts)
                        case L.find ((teamName ==) . githubTeamName) teams of
                          Nothing -> do $logWarn $ T.pack $ "There are no team: " ++ teamName
                                        $logInfo $ T.pack $ "Teams: " ++ L.intercalate ", " (L.map githubTeamName teams)
                                        return []
                          Just t  -> Github.membersOfTeam auth (githubTeamId t)
    Nothing -> Github.membersOf auth (view githubOrg opts)
  where auth = Just $ view githubAuth opts

getData :: (MonadHTTP m, MonadLogger m, MonadThrow m) => Opts -> m ([Fum.User], [GithubOwner])
getData opts =
    (,) <$> Fum.getAllUsers (view fumAuthToken opts) (view fumApiUsersUrl opts)
        <*> getGithubUsers opts

applyLoggingCaching :: (MonadIO m', MonadHTTP m', MonadMask m') => Opts -> (forall m. (MonadHTTP m, MonadLogger m, MonadThrow m) => m a) -> m' a
applyLoggingCaching opts m =
  case (view optsLogging opts, view optsCaching opts) of
    (True, True)    -> flip runCachingT (responseDirectoryCache $ view optsCacheDir opts) . runStderrLoggingT $ m
    (True, False)   -> runStderrLoggingT m
    (False, True)   -> flip runCachingT (responseDirectoryCache $ view optsCacheDir opts) . runNoLoggingT $ m
    (False, False)  -> runNoLoggingT m

defailedOwnerInfo :: DetailedOwner -> String
defailedOwnerInfo owner = detailedOwnerLogin owner ++ n ++ e
  where n = maybe "" (" : " ++) $ mfilter (/= "") $ detailedOwnerName owner
        e = maybe "" (" : " ++) $ mfilter (/= "") $ detailedOwnerEmail owner

getDetailedOwner :: Opts -> GithubOwner -> IO DetailedOwner
getDetailedOwner opts name = applyLoggingCaching opts $ Github.userInfoFor (Just $ view githubAuth opts) $ githubOwnerLogin name

-- We need this in IO to use 'swarm'
getDetailedOwners :: Opts -> [GithubOwner] -> IO [DetailedOwner]
getDetailedOwners opts = swarm (getDetailedOwner opts)

main' :: Opts -> IO ()
main' opts = do
    (fum, github) <- applyLoggingCaching opts $ getData opts
    let users = Compare.gitHubUsersNotInFum github fum
    detailedOwners <- getDetailedOwners opts users
    putStrLn $ (show . L.length $ users) ++ "/" ++ (show . L.length $ github) ++ " members of the GitHub organization '" ++ _githubOrg opts ++ "' are not in FUM:"
    putStrLn $ show $ view team opts
    mapM_ (putStrLn . defailedOwnerInfo) detailedOwners

-- Orphans

instance NFData GithubDate where
  rnf (GithubDate d) = rnf d

instance NFData DetailedOwner where
  rnf (DetailedUser a b c d e f g h i j k l m n o p r s q) = rnf (a, b, c, d, e) `seq`
                                                             rnf (f, g, h, i, j) `seq`
                                                             rnf (k, l, m, n, o) `seq`
                                                             rnf (p, r, s, q)
  rnf (DetailedOrganization a b c d e f g h i j k l m n o p) = rnf (a, b, c, d, e) `seq`
                                                               rnf (f, g, h, i, j) `seq`
                                                               rnf (k, l, m, n, o) `seq`
                                                               rnf p
