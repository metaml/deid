{-# language OverloadedRecordDot #-}
{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
module Main where

import Control.Lens ((^?), (^.))
import Data.Aeson.Lens as L
import Data.Aeson.Types (emptyObject)
import Data.Either
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text as T
import Database.Bloodhound hiding (key)
import Gogol.DLP.Types
import Model.Deid
import Model.Elastic
import Prelude as P
import Streamly.Prelude as S

type DeidTuple = ( DocId
                 , Maybe Text
                 , Maybe Text
                 , Maybe Text
                 , Maybe Text
                 )

main :: IO ()
main = do
  let s = server "localhost" 9200
      docs = 99
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
    & S.map (\(id', obj) -> (id', fromMaybe emptyObject obj))
    & S.map (\(id', obj) -> ( id'
                            , obj ^? L.key "lp_owner" . _String
                            , obj ^? L.key "service_name" . _String
                            , obj ^? L.key "message" . _String
                            , obj ^? L.key "@timestamp" . _String
                            )
            )
    & S.map toDeid
    & S.mapM (\case
                 Right l -> if (l ^. message) /= ""
                            then do
                              r <- inspectContent (l ^. message) "projects/lpgprj-gss-p-ctrlog-gl-01/locations/us-east1"
                              pure $ Right (r, l)
                            else
                              pure $ Left (T.pack . show $ l)
                 Left e -> pure $ Left e
             )
    & S.mapM (\case
                 Right (GooglePrivacyDlpV2InspectContentResponse r, l) ->
                   case r of
                     Just (GooglePrivacyDlpV2InspectResult (Just fs) _) -> pure $ (\f -> (f, l)) <$> fs
                     Just (GooglePrivacyDlpV2InspectResult Nothing _)   -> pure []
                     Nothing -> pure []
                 Left e -> putStrLn (show e) >> pure []
             )
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable
    & S.map (\(f, Log id' lpo sn _ t) -> ( id'
                                         , lpo
                                         , sn
                                         , f.quote
                                         , f.infoType
                                         , f.likelihood
                                         , t
                                         , f
                                         )
            )
    & S.map (\(id', lpo, sn, q, it, l, t, f) -> ( id'
                                                , lpo
                                                , sn
                                                , q
                                                , case it of
                                                    Just it' -> it'.name
                                                    Nothing  -> Nothing
                                                , l
                                                , t
                                                , f
                                                )
            )
    & S.mapM print
    & S.drain
  where
    toDeid :: DeidTuple -> Either Text Log
    toDeid (id', Just lo, Just sn, Just msg, Just ts) = Right $ Log id' lo sn msg ts
    toDeid tuple = Left $ (T.pack . show) tuple
