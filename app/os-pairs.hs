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
  let url = Es.server arg'.server arg'.port
  indices <- case arg'.query of
               Cli.Query Cli.IndicesAll    -> currentIndexes url
               Cli.Query (Cli.Indices ixs) -> pure $ (IndexName <$> Set.toList ixs)

  S.fromList indices
    & S.filter (\(IndexName i) -> not (T.isPrefixOf "." i))
    & S.trace (\(IndexName i) -> if arg'.verbose then (hPutStrLn stderr i) else pure ())
    & S.mapM (\i -> documents url i 0 arg'.maxResults)
    & S.map rights
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable -- transform a stream of lists to a stream of elements
    & S.map searchHits
    & S.map hits
    & S.concatMap S.fromFoldable
    & S.map (\h -> (hitDocId h, hitSource h))
    & S.map (\(id', o) -> (id', fromMaybe emptyObject o))
    & S.map (\(id', o) -> (id', select "lp_owner" o, select "service_name" o, select "@timestamp" o))
    & S.filter (\(_, o, n, _) -> isJust o && isJust n)
    & S.map (\(_, o, n, _) -> (fromJust o, fromJust n))
    & S.mapM (\t -> T.putStr $ (toStrict . decodeUtf8 . encode) [t])
    & S.drain
  where
    select :: A.Key -> Value -> Maybe Text
    select k o =  o ^? A.key k . _String
