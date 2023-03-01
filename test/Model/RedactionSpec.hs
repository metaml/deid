module Model.RedactionSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "email" $ do
    it "matches" $ do
      "1" `shouldBe` "1"
