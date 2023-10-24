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
import Streamly.Data.Stream as S
import System.IO (stderr)
import qualified Data.Set as Set
import qualified Etc.Deid as Cli
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as U

main :: IO ()
main = do
  arg' <- Cli.arg

  let esUrl = Es.server arg'.server arg'.port
  indices <- case arg'.query of
               Cli.Query Cli.IndicesAll    -> currentIndexes esUrl
               Cli.Query (Cli.Indices ixs) -> pure $ (IndexName <$> Set.toList ixs)

  S.fromList indices
    & S.trace (\(IndexName i) -> if arg'.verbose then (hPutStrLn stderr i) else pure ())
    & S.mapM (\i -> documents esUrl i 0 arg'.maxResults)
    & fmap rights
    & S.filter (not . P.null)
    & S.unfoldMany U.fromList    
    & fmap searchHits
    & fmap hits
    & S.unfoldMany U.fromList
    & fmap (\h -> (hitDocId h, hitSource h))
    & fmap (\(id', o) -> (id', fromMaybe emptyObject o))
    & fmap (\(id', o) -> (id', select "message" o, select "@timestamp" o))
    & S.filter (\(_, m, t) -> isJust m && isJust t)
    & fmap (\(DocId id', m, t) -> (id', fromJust m, fromJust t))
    & fmap (\(id', m, t) -> (id', T.length m, t))
    & fmap (\t@(id', l, ts) -> (id', l, ts, encode [t]))
    & fmap (\(id', l, ts, t) -> (id', l, ts, (toStrict . decodeUtf8) t))
    & S.mapM (\(_, _, _, t) -> T.putStr t)
    & S.fold F.drain

  where
    select :: A.Key -> Value -> Maybe Text
    select k o =  o ^? A.key k . _String
