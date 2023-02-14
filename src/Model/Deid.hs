module Model.Deid where

import Control.Lens
import Data.Aeson
import Data.Csv (Header, ToField, ToRecord, record, toField, toRecord)
import Data.Vector as V
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Bloodhound hiding (key)
import GHC.Generics
import Gcp.Send
import Gogol (Logger)
import Gogol.DLP
import Gogol.DLP.Types
import Gogol.Prelude

type Message = Text
type Parent = Text
type LpOwner = Text
type ServiceName = Text
type Quote = Text
type Timestamp = Text

data Log = Log { _docId :: DocId
               , _lpOwner :: Text
               , _serviceName :: Text
               , _message :: Text
               , _timestamp :: Text
               , _quote :: Maybe Text
               , _infoType :: Maybe Text
               , _likelihood :: Maybe Text
               , _quoteRange :: Maybe (Int, Int)
               }
         deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance ToRecord Log where
  toRecord l = record [ toField l._lpOwner
                      , toField l._serviceName
                      , toField l._quote
                      , toField l._infoType
                      , toField l._likelihood
                      , toField $ "[" <> (sample l) <> "]"
                      , toField l._timestamp
                      , toField l._docId
                      ]

instance ToField DocId where
  toField (DocId id') = encodeUtf8 id'

-- @todo: this is gross--derive from ToRecord Log
header' :: [Text]
header' = [ "doc_id"
          , "lp_owner"
          , "service_name"
          , "quote"
          , "info_type"
          , "liklihood"
          , "quote_context"
          , "timestamp"
          ]

inspectLog :: Either Text Log -> IO (Either Text (GooglePrivacyDlpV2InspectContentResponse, Log))
inspectLog e = infoLogger >>= \l -> inspectLogWithLogger l e

inspectLogDebug :: Either Text Log -> IO (Either Text (GooglePrivacyDlpV2InspectContentResponse, Log))
inspectLogDebug e = debugLogger >>= \l -> inspectLogWithLogger l e

inspectLogWithLogger :: Logger -> Either Text Log -> IO (Either Text (GooglePrivacyDlpV2InspectContentResponse, Log))
inspectLogWithLogger logger = \case
  Right l -> if T.null l._message
             then pure $ Left $ pack . show $ l
             else do
               let msg = inspectContentReq (l._message)
                   parent = "projects/lpgprj-gss-p-ctrlog-gl-01/locations/us-east1"
                   inspection = newDLPProjectsLocationsContentInspect parent msg
                   scope = Proxy :: Proxy (Scopes DLPProjectsLocationsContentInspect)
               r <- sendGcpWithLogger logger inspection scope
               pure $ Right (r, l)
  Left e' -> pure $ Left e'

inspect :: Text -> IO GooglePrivacyDlpV2InspectContentResponse
inspect l = do
  let msg = inspectContentReq l
      parent = "projects/lpgprj-gss-p-ctrlog-gl-01/locations/us-east1"
      inspection = newDLPProjectsLocationsContentInspect parent msg
      scope = Proxy :: Proxy (Scopes DLPProjectsLocationsContentInspect)
  logger <- infoLogger
  sendGcpWithLogger logger inspection scope

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

-- NB: refactor later
deidentify :: Text -> IO GooglePrivacyDlpV2DeidentifyContentResponse
deidentify l = do
  let msg = deidentifyContentReq l
      parent = "projects/lpgprj-gss-p-ctrlog-gl-01/locations/us-east1"
      deidentification = newDLPProjectsLocationsContentDeidentify parent msg
      scope = Proxy :: Proxy (Scopes DLPProjectsLocationsContentDeidentify)
  logger <- infoLogger
  sendGcpWithLogger logger deidentification scope

deidentifyContentReq :: Text -> GooglePrivacyDlpV2DeidentifyContentRequest
deidentifyContentReq v = newGooglePrivacyDlpV2DeidentifyContentRequest { item = Just i
                                                                       , deidentifyConfig = Just dc
                                                                       , inspectConfig = Just ic
                                                                       }
  where i = newGooglePrivacyDlpV2ContentItem { value = Just v } :: GooglePrivacyDlpV2ContentItem
        dc = newGooglePrivacyDlpV2DeidentifyConfig { infoTypeTransformations = Just itts
                                                   } :: GooglePrivacyDlpV2DeidentifyConfig;
        itts = newGooglePrivacyDlpV2InfoTypeTransformations { transformations = Just [itt]
                                                            }
        itt = newGooglePrivacyDlpV2InfoTypeTransformation { infoTypes = Just its
                                                          , primitiveTransformation = Just pt
                                                          }
        pt = newGooglePrivacyDlpV2PrimitiveTransformation { replaceWithInfoTypeConfig = Just GooglePrivacyDlpV2ReplaceWithInfoTypeConfig
                                                          }
        ic = newGooglePrivacyDlpV2InspectConfig { infoTypes = Just its
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

sample :: Log -> Text
sample l = let q = quoteRange' l._quoteRange l._message l._quote
           in case q of
                Just qr -> T.take (snd qr - fst qr) (T.drop (fst qr) l._message)
                Nothing -> ""

quoteRange' :: Maybe (Int, Int) -> Text -> Maybe Quote -> Maybe (Int, Int)
quoteRange' Nothing _ _ = Nothing
quoteRange' (Just (s, e)) msg (Just q) = let mLen = T.length msg
                                             qLen = max 11 (T.length q)
                                         in Just $ ((max (s - qLen) 0), (min (e + qLen) mLen))
quoteRange' (Just (s, e)) msg Nothing  = let mLen = T.length msg
                                             qLen = 0
                                         in Just $ ((max (s - qLen) 0), (min (e + qLen) mLen))
