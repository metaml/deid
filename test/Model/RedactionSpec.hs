module Model.RedactionSpec where

import Data.Text
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
      jsonWebToken ("yadda yadda " <> jwt <> " yadda") `shouldBe` "yadda yadda [JSON_WEB_TOKEN] yadda"
      accessToken "yadda yadda access_token=asdfsdfdfdfdasdfefasf09 yadda" `shouldBe` "yadda yadda [ACCESS_TOKEN] yadda"
      accessToken "yadda yadda accessToken=asdfsdfdfdfdasdfefasf09 yadda" `shouldBe` "yadda yadda [ACCESS_TOKEN] yadda"
      bearerToken ("yadda yadda " <> "bearerToken: " <> tok <> " yadda") `shouldBe` "yadda yadda [BEARER_TOKEN] yadda"
      bearerToken ("yadda yadda " <> "bearerToken:  " <> tok <> " yadda") `shouldBe` "yadda yadda [BEARER_TOKEN] yadda"
      bearerToken ("yadda yadda " <> "\"bearerToken\": " <> tok <> " yadda") `shouldBe` "yadda yadda [BEARER_TOKEN] yadda"
      bearerToken ("yadda yadda " <> "\"bearerToken\":  " <> tok <> " yadda") `shouldBe` "yadda yadda [BEARER_TOKEN] yadda"
      urlToken ("yadda yadda http://foo:bar@baz.com/ yadda") `shouldBe` "yadda yadda http:[URL_TOKEN]baz.com/ yadda"

jwt :: Text
jwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"

tok :: Text
tok = "AbCdEf123456"

tok256 :: Text
tok256 = ""
