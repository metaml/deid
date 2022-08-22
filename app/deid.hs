module Main where

import Control.Lens ((^?))
import Data.Aeson.Lens as L
import Data.Aeson.Types (emptyObject)
import Data.Either
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text as T
import Database.Bloodhound hiding (key)
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
  is <- currentIndexes s
  S.fromList is
    & S.mapM (\i -> print i >> pure i)
    & S.mapM (\i -> documents s i 0 3)
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
    & S.mapM print
    & S.drain
  where
    toDeid :: DeidTuple -> Either Text Log
    toDeid (id', Just lo, Just sn, Just msg, Just ts) = Right $ Log id' lo sn msg ts
    toDeid tuple = Left $ "error: " <> (T.pack . show $ tuple)
