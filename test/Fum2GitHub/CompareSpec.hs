module Fum2GitHub.CompareSpec (
    spec,
) where

import qualified Fum2GitHub.Compare as Compare
import qualified Fum2GitHub.Fum as Fum
import           Github.Extra as GitHub
import           Test.Hspec (describe, it, shouldBe, Spec)

mockOrgMember :: String -> GithubOwner
mockOrgMember login = GithubUser
  { githubOwnerLogin = login
  , githubOwnerAvatarUrl = ""
  , githubOwnerUrl = ""
  , githubOwnerId = 1
  , githubOwnerGravatarId = Nothing
  }

spec :: Spec
spec = do
  describe "gitHubUsersNotInFum" $ do
    it "shows everyone if no FUM users" $ do
      let github = map mockOrgMember ["some", "user", "name"]
          fum = []
          result = github
      Compare.gitHubUsersNotInFum github fum `shouldBe` result

    it "shows no one if all are in FUM" $ do
      let github = map mockOrgMember ["andy", "tom"]
          fum = [Fum.User{Fum.userGithub=g,
                          Fum.userName="", Fum.userEmail=Nothing} |
                 g <- [Just "paul", Just "tom", Nothing, Just "andy"]]
          result = []
      Compare.gitHubUsersNotInFum github fum `shouldBe` result

    it "compares case-insensitive" $ do
      let github = map mockOrgMember ["TestUser", "stranger", "otherUser"]
          fum = [Fum.User{Fum.userGithub=g, Fum.userName="",
                          Fum.userEmail=Nothing} |
                 g <- [Just "TESTuser", Nothing, Just "otheruseR"]]
          result = map mockOrgMember ["stranger"]
      Compare.gitHubUsersNotInFum github fum `shouldBe` result
