module Model.Redaction where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text

email :: Text -> Text
email = set ([regex|\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+|] . match) "[EMAIL]"

phone :: Text -> Text
phone = set ([regex|number: (\+?\(?\d{1,4}\)?([-. ]{1})?\d{3}([-. ]{1})?\d{3,7})|] . match) "[PHONE]"

ip :: Text -> Text
ip = set ([regex|address: ((?:\d{1,3}\.){3}\d{1,3}|(?:[0-9a-f]{1,4}[\.:]){7}[0-9a-f]{1,4})|] . match) "[IP]"

jsonWebToken :: Text -> Text
jsonWebToken = set ([regex|([\w-]|\.|\:)*\.([\w-]|\.|\:)*\.([\w-]|\.|\:)*|] . match) "[JSON_WEB_TOKEN]"

accessToken :: Text -> Text
accessToken = set ([regex|(access_token|accessToken)=[A-Za-z0-9_.-]+|] . match) "[ACCESS_TOKEN]"

bearerToken :: Text -> Text
bearerToken = set ([regex|(bearerToken: *|"bearerToken": *|Authorization: +Bearer +|Bearer +)("?)[A-Za-z0-9_.-]+("?)|] . match) "[BEARER_TOKEN]"

urlToken :: Text -> Text
urlToken = set ([regex|(/)[A-Za-z0-9_.-]{256,}}(/)|] . match) "[URL_TOKEN]"

creditCard :: Text -> Text
creditCard = set ([regex|('cardNumber': *|')}(/)|] . match) "[URL_TOKEN]"

-- Email:          \w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+
-- Phone           number: (\+?\(?[0-9]{1,4}\)?([-. ]{1})?[0-9]{3}([-. ]{1})?[0-9]{3,7})
-- IP              address: ((?:[0-9]{1,3}\.){3}[0-9]{1,3}|(?:[0-9a-z]{1,4}[\.:]){7}[0-9a-z]{1,4})
-- JSON Web Token: ([\w-]|\.|\:)*\.([\w-]|\.|\:)*\.([\w-]|\.|\:)*
-- Access Token:   (access_token=)[a-zA-Z0-9_.-]+
--                 (accessToken=)[a-zA-Z0-9_.-]+
-- Bearer Token:   ("?bearerToken"?: *"?)[a-zA-Z0-9_.-]+("?)
--                 (Authorization: +Bearer +)[a-zA-Z0-9_.-]+'
--                 (Bearer +)[a-zA-Z0-9_.-]+', "\1[JWT]
-- URL Tokens:     (/)[a-zA-Z0-9_.-]{256,}(/)
-- Card Number     ('cardNumber': *')[0-9]+(')
--                 (\\*"cardNumber\\*": *\\*")[0-9]+(\\*")
-- CardCType:      ('cardCtype': *')[a-zA-Z-]+(')
--                 (\\*"cardCtype\\*": *\\*")[a-zA-Z-]+(\\*")
-- Card Name:      ('cardName': *')[a-zA-Z0-9 -]+(')
--                 (\\*"cardName\\*": *\\*")[a-zA-Z0-9 -]+(\\*")
