import qualified Fum2GitHub.Fum as Fum
import qualified Fum2GitHub.GitHub as GitHub
import           Fum2GitHub.Util (URL(URL))
import           System.Environment (getArgs)
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fumApiUsersUrl, fumAuthToken, gitHubOrg, gitHubOAuthToken] -> do
      printFumUsers (URL fumApiUsersUrl) (Fum.AuthToken fumAuthToken)
      printGitHubUsers gitHubOrg (GitHub.OAuthToken gitHubOAuthToken)
    _ -> do
      hPutStrLn stderr "Usage: fum-api-users-url fumAuthToken gitHubOrg gitHubOAuthToken"
      exitWith $ ExitFailure 1


printFumUsers :: URL -> Fum.AuthToken -> IO ()
printFumUsers fumApiUsersUrl authToken = do
    usersE <- Fum.getAllUsers fumApiUsersUrl authToken
    case usersE of
      Left msg -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right users -> do
        print . filter (not . null . Fum.github) $ users

printGitHubUsers :: String -> GitHub.OAuthToken -> IO ()
printGitHubUsers orgName oAuthToken = do
    usersE <- GitHub.getOrgMembers orgName oAuthToken
    case usersE of
      Left msg -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right users -> do
        print $ map GitHub.getOrgMember users
