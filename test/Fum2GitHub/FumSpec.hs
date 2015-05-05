{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.FumSpec (
    spec,
) where

import qualified Data.Aeson as Aeson
import qualified Fum2GitHub.Fum as Fum
import           Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "userFromAPI" $ do
    it "reads .username and .github from the FUM API's JSON response" $ do
      let dump = "{\"username\": \"jsmi\", \"github\": \"john\"}"
      case Aeson.decode dump :: Maybe Aeson.Value
        of Just v -> Fum.userFromAPI v `shouldBe` Right Fum.User{
                       Fum.username="jsmi", Fum.github="john"}
