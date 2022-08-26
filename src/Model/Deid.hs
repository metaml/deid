module Model.Deid where

import Control.Lens
import Data.Aeson
import Data.Text
import Database.Bloodhound hiding (key)
import GHC.Generics
import Gcp.Send (send')
import Gogol.DLP
import Gogol.DLP.Types
import Gogol.Prelude

data Log = Log { _docId :: DocId
               , _lpOwner :: Text
               , _serviceName :: Text
               , _message :: Text
               , _timestamp :: Text
               , _quote :: Maybe Text
               , _infoType :: Maybe Text
               , _likelihood :: Maybe Text
               , _quoteRange :: Maybe (Int64, Int64)
               }
         deriving (Generic, Show, ToJSON, FromJSON)

makeLenses ''Log

type Message = Text
type Parent = Text

inspectLog :: Either Text Log -> IO (Either Text (GooglePrivacyDlpV2InspectContentResponse, Log))
inspectLog e = case e of
                 Right l -> if (l ^. message) /= ""
                            then do
                              r <- inspectContent (l ^. message) "projects/lpgprj-gss-p-ctrlog-gl-01/locations/us-east1"
                              pure $ Right (r, l)
                            else
                              pure $ Left (pack . show $ l)
                 Left e' -> pure $ Left e'

inspectContent :: Message -> Parent -> IO (Rs DLPProjectsLocationsContentInspect)
inspectContent msg par = do
  let p = inspectContentReq msg
      r = newDLPProjectsLocationsContentInspect par p
      ps = Proxy :: Proxy (Scopes DLPProjectsLocationsContentInspect)
  send' r ps

-- NB: odd record update error that requires a type signature
inspectContentReq :: Text -> GooglePrivacyDlpV2InspectContentRequest
inspectContentReq v = newGooglePrivacyDlpV2InspectContentRequest { item = Just i
                                                                 , inspectConfig = Just c
                                                                 }
  where i = newGooglePrivacyDlpV2ContentItem { value = Just v } :: GooglePrivacyDlpV2ContentItem
        c = newGooglePrivacyDlpV2InspectConfig { infoTypes = Just its
                                               , minLikelihood = Just GooglePrivacyDlpV2InspectConfig_MinLikelihood_Likely
                                               , excludeInfoTypes = Just False
                                               , includeQuote = Just True
                                               } :: GooglePrivacyDlpV2InspectConfig
        its = [ newGooglePrivacyDlpV2InfoType { name = Just "AGE" }                :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "CREDIT_CARD_NUMBER" } :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "DATE_OF_BIRTH" }      :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "EMAIL_ADDRESS" }      :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "GENDER" }             :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "PASSPORT" }           :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "PHONE_NUMBER" }       :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "STREET_ADDRESS" }     :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "AUTH_TOKEN" }         :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "AWS_CREDENTIALS" }    :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "AZURE_AUTH_TOKEN" }   :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "ENCRYPTION_KEY" }     :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "GCP_API_KEY" }        :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "GCP_CREDENTIALS" }    :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "JSON_WEB_TOKEN" }     :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "HTTP_COOKIE" }        :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "PASSWORD" }           :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "WEAK_PASSWORD_HASH" } :: GooglePrivacyDlpV2InfoType
              , newGooglePrivacyDlpV2InfoType { name = Just "XSRF_TOKEN" }         :: GooglePrivacyDlpV2InfoType
              ]
