module Main where

import Control.Lens ((^?), (.~))
import Data.Aeson.Key as A
import Data.Aeson.Lens as A
import Data.Aeson.Types (Value, emptyObject)
import Data.Either
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text as T
import Database.Bloodhound hiding (key)
import Gogol.DLP.Types
import Model.Deid
import Model.Elastic
import Prelude as P
import Streamly.Prelude as S

type LpOwner = Text
type ServiceName = Text
type LogMessage = Text
type Quote = Text
type Timestamp = Text

type DeidTuple = ( DocId
                 , Maybe LpOwner
                 , Maybe ServiceName
                 , Maybe LogMessage
                 , Maybe Timestamp
                 )

main :: IO ()
main = do
  let s = server "localhost" 9200
      docs = 3
  is <- currentIndexes s
  S.fromList is
    & S.mapM (\i -> print i >> pure i)
    & S.mapM (\i -> documents s i 0 docs)
    & S.map rights
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable -- transform stream of lists to stream of elements
    & S.map searchHits
    & S.map hits
    & S.concatMap S.fromFoldable
    & S.map (\h -> (hitDocId h, hitSource h))
    & S.map (\(id', o) -> (id', fromMaybe emptyObject o))
    & S.map (\(id', o) -> (id', select "lp_owner" o, select "service_name" o, select "message" o, select "@timestamp" o))
    & S.map toDeid
    & S.mapM inspectLog
    & S.mapM toFindings
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable
    & S.map (\(f, l) -> (f.quote, f.infoType, f.likelihood, f.location, l))
    & S.filter (\(q, _, _, _, _) -> isJust q)
    & S.map (\(q, i, l, loc, log') -> (fromJust q, fromJust i, fromJust l, fromJust loc, log'))
    & S.map (\(q, i, l, loc, log') -> ( loc.codepointRange
                                      , log' & quote .~ Just q
                                             & infoType .~ i.name
                                             & likelihood .~ Just l.fromGooglePrivacyDlpV2Finding_Likelihood
                                      )
            )
    & S.map (\(cpr, log') -> case cpr of
                               Just cpr' -> log' & (quoteRange .~ if isJust cpr'.start && isJust cpr'.end
                                                                  then Just ( (fromIntegral . fromJust) cpr'.start
                                                                            , (fromIntegral . fromJust) cpr'.end
                                                                            )
                                                                  else Nothing
                                                   )
                               Nothing -> log' & quoteRange .~ Nothing
            )
    & S.map (\l -> case (l._quoteRange) of
                     Just qr -> l & quoteRange .~ (Just (quoteRange' qr l._message l._quote))
                     Nothing -> l
            )
    & S.mapM print
    & S.drain

toDeid :: DeidTuple -> Either Text Log
toDeid (id', Just lo, Just sn, Just msg, Just t) = Right $ Log id' lo sn msg t Nothing Nothing Nothing Nothing
toDeid tuple = Left $ (T.pack . show) tuple

toFindings :: Either Text (GooglePrivacyDlpV2InspectContentResponse, Log) -> IO [(GooglePrivacyDlpV2Finding, Log)]
toFindings = \case
  Right (GooglePrivacyDlpV2InspectContentResponse r, l) ->
    case r of
      Just (GooglePrivacyDlpV2InspectResult (Just fs) _) -> pure $ (\f -> (f, l)) <$> fs
      Just (GooglePrivacyDlpV2InspectResult Nothing _)   -> pure []
      Nothing                                            -> pure []
  Left e' -> print e' >> pure []

select :: A.Key -> Value -> Maybe Text
select k o =  o ^? A.key k . _String

quoteRange' :: (Int, Int) -> LogMessage -> Maybe Quote -> (Int, Int)
quoteRange' (s, e) msg (Just q) = let mLen = T.length msg
                                      qLen = T.length q
                                  in (fromIntegral (max (s - qLen) 0), fromIntegral (min (e + qLen) mLen))
quoteRange' (s, e) msg Nothing = let mLen = T.length msg
                                     qLen = 0
                                 in (fromIntegral (max (s - qLen) 0), fromIntegral (min (e + qLen) mLen))
