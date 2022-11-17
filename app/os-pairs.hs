module Main where

import Control.Lens ((^?))
import Data.Aeson.Key as A
import Data.Aeson.Lens as A
import Data.Aeson.Types (Value, emptyObject)
import Data.Csv
import Data.Either
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.IO as T
import Database.Bloodhound hiding (key)
import Etc.Deid
import Model.Elastic as Es
import Prelude as P
import Streamly.Prelude as S
import System.IO (stderr)
import qualified Data.Set as Set
import qualified Etc.Deid as Cli

main :: IO ()
main = do
  arg' <- Cli.arg
  let esUrl = Es.server arg'.server arg'.port

  S.fromPure esUrl
    & S.mapM (\url -> ownerServiceNames url)
    & S.map rights
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable -- transform a stream of lists to a stream of elements
    & S.map searchHits
    & S.map hits
    & S.concatMap S.fromFoldable
    & S.map (\h -> (hitDocId h, hitSource h))
    & S.map (\(id', o) -> (id', fromMaybe emptyObject o))
    & S.map (\(id', o) -> (id', select "message" o, select "@timestamp" o))
    & S.filter (\(_, m, t) -> isJust m && isJust t)
    & S.map (\(DocId id', m, t) -> (id', fromJust m, fromJust t))
    & S.map (\(id', m, t) -> (id', T.length m, t))
    & S.map (\t@(id', l, ts) -> (id', l, ts, encode [t]))
    & S.map (\(id', l, ts, t) -> (id', l, ts, (toStrict . decodeUtf8) t))
    & S.mapM (\(_, _, _, t) -> T.putStr t)
    & S.drain

  where
    select :: A.Key -> Value -> Maybe Text
    select k o =  o ^? A.key k . _String
