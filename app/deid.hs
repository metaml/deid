module Main where

import Control.Lens ((^?))
import Data.Aeson.Types (emptyObject)
import Data.Either
import Data.Function ((&))
import Data.Maybe
import Database.Bloodhound hiding (key)
import Model.Elastic
import Prelude as P
import Streamly.Prelude as S
import qualified Data.Aeson.Lens as L

main :: IO ()
main = do
  let s = server "localhost" 9200
  is <- currentIndexes s
  S.fromList is
    & S.mapM (\i -> print i >> pure i)
    & S.mapM (\i -> documents s i 0 3)
    & S.map rights
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable -- Stream of lists to stream of elements
    & S.map searchHits
    & S.map hits
    & S.concatMap S.fromFoldable
    & S.map (\h -> (hitDocId h, hitSource h))
    & S.map (\(dId, obj) -> (dId, fromMaybe emptyObject obj))
    & S.map (\(dId, obj) -> (dId, obj ^? L.key "lp_owner", obj ^? L.key "service_name", obj ^? L.key "message", obj ^? L.key "@timestamp"))
    & S.mapM print
    & S.drain
  -- S.fromList ixs
  --   & S.mapM (\i -> print i >> pure i)
  --   & S.mapM (\i -> documents s i 0 3)
  --   & S.map rights
  --   & S.filter (not . P.null)
  --   & S.map (\rs -> searchHits <$> rs)
  --   & S.map (\shs -> P.head (hits <$> shs))
  --   & S.map (\hs -> zip (hitDocId <$> hs) (hitSource <$> hs))
  --   & S.map (\ps -> (\(DocId i, o) -> (i, fromMaybe emptyObject o)) <$> ps)
  --   & S.map (\ps -> (\(i, o) -> (i, o ^? L.key "lp_owner", o ^? L.key "service_name", o ^? L.key "message")) <$> ps)
  --   & S.mapM print
  --   & S.drain
