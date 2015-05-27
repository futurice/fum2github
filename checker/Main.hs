module Main (main) where

import           Data.Maybe
import qualified Fum2GitHub.Compare as Compare
import qualified Fum2GitHub.Fum as Fum
import qualified Fum2GitHub.GitHub as GitHub
import           Fum2GitHub.Types(URL(URL))
import           Options.Applicative
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr)
import           System.Log.Logger

-- fumApiUsersUrl fumAuthToken githubOrganisation githubOAuthtoken
data Opts = Opts URL Fum.AuthToken String GitHub.OAuthToken

opts :: Parser Opts
opts = Opts <$> (URL <$> argument str (metavar "fum-api-users-url"))
            <*> (Fum.AuthToken <$> argument str (metavar "fum-auth-token"))
            <*> argument str (metavar "github-organisation")
            <*> (GitHub.OAuthToken <$> argument str (metavar "github-oauth-token"))

main :: IO ()
main = execParser opts' >>= main'
  where opts' = info (helper <*> opts) (fullDesc <> header "Print fum and github users")

main' :: Opts -> IO ()
main' (Opts fumApiUsersUrl fumAuthToken githubOrg githubOAuthToken) = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    fumE <- Fum.getAllUsers fumApiUsersUrl fumAuthToken
    githubE <- GitHub.getOrgMembers githubOrg githubOAuthToken
    let diffE :: Either String [GitHub.OrgMember]
        diffE = do
          fum <- fumE
          github <- githubE
          return $ Compare.gitHubUsersNotInFum github fum
    case diffE of
      Left msg -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right users -> do
        putStrLn $ (show . length $ users) ++ " members of the " ++
          githubOrg ++ " GitHub organization are not in FUM:"
        mapM_ (putStrLn . GitHub.getOrgMember) users
