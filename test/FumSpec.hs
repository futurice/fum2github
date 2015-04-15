module FumSpec
(spec)
where

import Fum
import Test.Hspec

spec :: Spec
spec = do
  describe "fumAdd" $ do
    it "1+2 is 3" $ do
      fumAdd 1 2 `shouldBe` 3
