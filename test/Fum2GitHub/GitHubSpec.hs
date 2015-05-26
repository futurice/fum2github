{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.GitHubSpec (
    spec,
) where

import qualified Fum2GitHub.GitHub as GitHub
import           Fum2GitHub.Types (URL(URL))
import           Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "getNextUrl" $ do
    it "returns Nothing if no Link headers" $ do
      let hdrs = [("Some-Header", "some value"), ("Other Header", "other val")]
      GitHub.getNextUrl hdrs `shouldBe` Nothing
    it "returns the first rel=next value" $ do
      let hdrs = [
           ("Dummy", "<urlA>; rel=\"next\", <urlB>; rel=\"last\""),
           ("Link", "<urlC>; rel=\"first\", <urlD>; rel=\"last\""),
           ("Link", "<urlE>; rel=\"first\", <urlF>; rel=\"next\", <urlG>; rel=\"prev\", <urlH>; rel=\"next\""),
           ("Link", "<urlI>; rel=\"next\"")
           ]
      GitHub.getNextUrl hdrs `shouldBe` Just (URL "urlF")
