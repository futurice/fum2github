module Fum2GitHub.Compare (
    gitHubUsersNotInFum,
) where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import qualified Data.Set as Set
import qualified Fum2GitHub.Fum as Fum
import qualified Fum2GitHub.GitHub as GitHub

gitHubUsersNotInFum :: [GitHub.OrgMember] -> [Fum.User] -> [GitHub.OrgMember]
gitHubUsersNotInFum ghMembers fumUsers =
    filter (not . inFum) ghMembers
    where

    fumList :: [String]
    fumList = do
        user <- fumUsers
        let github = Fum.userGithub user
        guard $ isJust github
        return $ map toLower (fromJust github)

    fumSet :: Set.Set String
    fumSet = Set.fromList fumList

    inFum :: GitHub.OrgMember -> Bool
    inFum x = Set.member lowerName fumSet
              where
              lowerName :: String
              lowerName = map toLower $ GitHub.getOrgMember x
