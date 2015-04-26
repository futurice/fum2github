import qualified Fum2GitHub.Fum as Fum
import           System.Environment (getArgs)
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fumApiUsersUrl, authToken] -> printFumUsers fumApiUsersUrl authToken
    _ -> do
      hPutStrLn stderr "Usage: fum-api-users-url authToken"
      exitWith $ ExitFailure 1

printFumUsers :: String -> String -> IO ()
printFumUsers fumApiUsersUrl authToken = do
    usersE <- Fum.getAllUsers fumApiUsersUrl authToken
    case usersE of
      Left msg -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right users -> do
        print . filter (not . null . Fum.github) $ users
