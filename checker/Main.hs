module Main (main) where

import           Data.Maybe
import qualified Fum2GitHub.Fum as Fum
import qualified Fum2GitHub.GitHub as GitHub
import           Fum2GitHub.Util(URL(URL))
import           Options.Applicative
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr)

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
        main' (Opts fumApiUsersUrl fumAuthToken githubOrg githubOAuthToken) = do
          printFumUsers fumApiUsersUrl fumAuthToken
          printGitHubUsers githubOrg githubOAuthToken

printFumUsers :: URL -> Fum.AuthToken -> IO ()
printFumUsers fumApiUsersUrl authToken = do
    usersE <- Fum.getAllUsers fumApiUsersUrl authToken
    case usersE of
      Left msg -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right users -> do
        let users' = filter (isJust . Fum.userGithub) users
        mapM_ print users'

printGitHubUsers :: String -> GitHub.OAuthToken -> IO ()
printGitHubUsers orgName oAuthToken = do
    usersE <- GitHub.getOrgMembers orgName oAuthToken
    case usersE of
      Left msg -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right users -> do
        mapM_ (putStrLn . GitHub.getOrgMember) users
