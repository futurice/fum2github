module Fum2GitHub.CompareSpec (
    spec,
) where

import qualified Fum2GitHub.Compare as Compare
import qualified Fum2GitHub.Fum as Fum
import qualified Fum2GitHub.GitHub as GitHub
import           Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "gitHubUsersNotInFum" $ do
    it "shows everyone if no FUM users" $ do
      let github = map GitHub.OrgMember ["some", "user", "name"]
          fum = []
          result = github
      Compare.gitHubUsersNotInFum github fum `shouldBe` result

    it "shows no one if all are in FUM" $ do
      let github = map GitHub.OrgMember["andy", "tom"]
          fum = [Fum.User{Fum.userGithub=g,
                          Fum.userName="", Fum.userEmail=Nothing} |
                 g <- [Just "paul", Just "tom", Nothing, Just "andy"]]
          result = []
      Compare.gitHubUsersNotInFum github fum `shouldBe` result

    it "compares case-insensitive" $ do
      let github = map GitHub.OrgMember["TestUser", "stranger", "otherUser"]
          fum = [Fum.User{Fum.userGithub=g, Fum.userName="",
                          Fum.userEmail=Nothing} |
                 g <- [Just "TESTuser", Nothing, Just "otheruseR"]]
          result = map GitHub.OrgMember ["stranger"]
      Compare.gitHubUsersNotInFum github fum `shouldBe` result
