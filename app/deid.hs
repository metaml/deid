module Main where

import Control.Lens ((^?), (.~))
import Control.Monad (when)
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
import Etc.Deid
import Gogol.DLP.Types
import Model.Deid
import Model.Elastic as Es
import Prelude as P
import Streamly.Prelude as S
import System.IO (stderr)
import qualified Data.Set as Set
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
  let esUrl = Es.server arg'.server arg'.port
  indices <- case arg'.query of
               Cli.Query Cli.IndicesAll    -> currentIndexes esUrl
               Cli.Query (Cli.Indices ixs) -> pure $ (IndexName <$> Set.toList ixs)
  T.putStrLn $ T.intercalate "," header'
  S.fromList indices
    & S.trace (\(IndexName i) -> if arg'.verbose then (hPutStrLn stderr i) else pure ())
    & S.mapM (\i -> documents esUrl i 0 arg'.maxResults)
    & S.map rights
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable -- transform a stream of lists to a stream of elements
    & S.map searchHits
    & S.map hits
    & S.concatMap S.fromFoldable
    & S.map (\h -> (hitDocId h, hitSource h))
    & S.map (\(id', o) -> (id', fromMaybe emptyObject o))
    & S.map (\(id', o) -> (id', select "lp_owner" o, select "service_name" o, select "message" o, select "@timestamp" o))
    & S.map toDeid
    & S.mapM (if arg'.debug then inspectLog' else inspectLog) -- call GCP
    & S.mapM (\e -> case (toFindings e) of
                      Right ps -> pure ps
                      Left e'  -> when arg'.debug (hPutStrLn stderr e') >> pure []
             )
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
    & S.mapM (\l -> T.putStr $ (L.toStrict . T.decodeUtf8 . encode) [l])
    & S.drain
  where
    toDeid :: DeidTuple -> Either Text Log
    toDeid (id', Just lo, Just sn, Just msg, Just t) = Right $ Log id' lo sn msg t Nothing Nothing Nothing Nothing
    toDeid tuple = Left $ (T.pack . show) tuple

    toFindings :: Either Text (GooglePrivacyDlpV2InspectContentResponse, Log) -> Either Text [(GooglePrivacyDlpV2Finding, Log)]
    toFindings = \case
      Right (GooglePrivacyDlpV2InspectContentResponse r, l) ->
        case r of
          Just (GooglePrivacyDlpV2InspectResult (Just fs) _) -> Right $ (\f -> (f, l)) <$> fs
          Just (GooglePrivacyDlpV2InspectResult Nothing _)   -> Right []
          Nothing                                            -> Right []
      Left e' -> Left e'

    select :: A.Key -> Value -> Maybe Text
    select k o =  o ^? A.key k . _String
