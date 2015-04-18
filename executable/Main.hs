import qualified Fum2GitHub.Fum as Fum
import           System.Environment (getArgs)

main = do
    args <- getArgs
    if length args /= 2 then
        error "Usage: fum-api-users-url authToken"
    else
        return ()
    let [fumApiUsersUrl, authToken] = args

    results <- Fum.getAPIAll fumApiUsersUrl authToken
    let users = map Fum.userFromAPI results
    putStrLn . show . filter (not . null . Fum.github) $ users
