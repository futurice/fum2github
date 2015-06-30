{-# LANGUAGE OverloadedStrings #-}
module Fum2GitHub.FumSpec (
    spec,
) where

import qualified Data.Aeson as Aeson
import qualified Fum2GitHub.Fum as Fum
import           Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "FromJSON" $ do
    it "reads .username and .github from the FUM API's JSON response" $ do
      let dump = "{\"username\": \"jsmi\", \"github\": \"john\", \"email\": null}"
      case Aeson.eitherDecode dump of
        Right v -> v `shouldBe` Fum.User { Fum.userName   ="jsmi"
                                         , Fum.userGithub = Just "john"
                                         , Fum.userEmail  = Nothing
                                         }
        Left err -> fail err
