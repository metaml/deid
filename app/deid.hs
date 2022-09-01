module Main where

import Control.Lens ((^?), (.~))
import Data.Aeson.Key as A
import Data.Aeson.Lens as A
import Data.Aeson.Types (Value, emptyObject)
import Data.Csv
import Data.Either
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text as T
import Data.Text.Lazy.Encoding as T
import Data.Text.IO as T
import Database.Bloodhound hiding (key)
import Gogol.DLP.Types
import Model.Deid
import Model.Elastic
import Prelude as P
import Streamly.Prelude as S
import qualified Data.Text.Lazy as L
import qualified Etc.Deid as Cli

type DeidTuple = ( DocId
                 , Maybe LpOwner
                 , Maybe ServiceName
                 , Maybe Message
                 , Maybe Timestamp
                 )

main :: IO ()
main = do
  arg' <- Cli.arg
  let s = server arg'.server arg'.port
      docs = arg'.results
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
                                             & likelihood .~ Just (strip l.fromGooglePrivacyDlpV2Finding_Likelihood)
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
    & S.mapM (\l -> T.putStrLn $ (L.toStrict . T.decodeUtf8 . encode) [l])
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
  Left e' -> do
    -- print e'
    pure []

select :: A.Key -> Value -> Maybe Text
select k o =  o ^? A.key k . _String
