----------------------------------------------------------------------------
-- |
-- Module      :  Fum2GitHub.Compare
-- Copyright   :  (C) 2015 Futurice
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
 ----------------------------------------------------------------------------
module Fum2GitHub.Compare (
    gitHubUsersNotInFum
  ) where

import           Data.List.Extra
import           Data.Maybe
import qualified Data.Set as Set
import qualified Fum2GitHub.Fum as Fum
import           Github.Fxtra as Github

gitHubUsersNotInFum :: [GithubOwner] -> [Fum.User] -> [GithubOwner]
gitHubUsersNotInFum ghMembers fumUsers =
    filter (not . inFum) ghMembers
    where

    -- Github Logins of users in FUM
    fumList :: [String]
    fumList = mapMaybe (fmap lower . Fum.userGithub) fumUsers

    fumSet :: Set.Set String
    fumSet = Set.fromList fumList

    inFum :: GithubOwner -> Bool
    inFum x = Set.member (lower . githubOwnerLogin $ x) fumSet
