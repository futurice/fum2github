module Fum2GitHub.FumSpec (
    spec,
) where

import           Fum2GitHub.Fum (User(User), username, github, userFromAPI)
import           Test.Hspec (describe, it, shouldBe, Spec)
import qualified Text.JSON as JSON

spec :: Spec
spec = do
  describe "userFromAPI" $ do
    it "reads .username and .github from the FUM API's JSON response" $ do
      case JSON.decode "{\"username\": \"jsmi\", \"github\": \"john\"}"
        of JSON.Ok j -> userFromAPI j `shouldBe` User{username="jsmi",
                                                       github="john"}
