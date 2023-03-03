module Model.RedactionSpec where

import Model.Redaction
import Test.Hspec

spec :: Spec
spec = do
  describe "redaction" $ do
    it "redacts" $ do
      email "yadda yadda foo@gmail.com yadda" `shouldBe` "yadda yadda [EMAIL] yadda"
      phone "yadda yadda number: 1-555-5555555 yadda" `shouldBe` "yadda yadda [PHONE] yadda"
      phone "yadda yadda number: +1-555-5555555 yadda" `shouldBe` "yadda yadda [PHONE] yadda"
      ip "yadda yadda address: 10.0.0.10 yadda" `shouldBe` "yadda yadda [IP] yadda"
      accessToken "yadda yadda access_token=asdfsdfdfdfdasdfefasf09 yadda" `shouldBe` "yadda yadda [ACCESS_TOKEN] yadda"
      accessToken "yadda yadda accessToken=asdfsdfdfdfdasdfefasf09 yadda" `shouldBe` "yadda yadda [ACCESS_TOKEN] yadda"
