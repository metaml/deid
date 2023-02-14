module Main where

import Control.Lens ((^?))
import Control.Monad (when)
import Data.Aeson.Key as A
import Data.Aeson.Lens as A
import Data.Aeson.Types (Value, emptyObject)
import Data.Csv
import Data.Either
import Data.Function ((&))
import Data.IORef
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lazy.Encoding as T
import Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int as B
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
import qualified Etc.Redact as Cli

main :: IO ()
main = do
  arg' <- Cli.arg
  let esUrl = Es.server arg'.server arg'.port

  S.fromList [(arg'.index, arg'.docId)]
    & S.map (\(i, d) -> (IndexName i, DocId d))
    & S.trace (\t -> if arg'.verbose then print t else pure ())
    & S.mapM (\(i, d) -> document esUrl i d)
    & S.map rights
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable
    & S.map searchHits
    & S.map hits
    & S.concatMap S.fromFoldable
    & S.trace (\h -> if arg'.verbose then print h else pure ())
    & S.map (\h -> (hitSource h, h))
    & S.map (\(src, h) -> (fromMaybe emptyObject src, h))
    & S.map (\(src, h) -> (src, select "message" src, h))
    & S.filter (\(_, msg, _) -> msg /= Nothing)
    & S.map (\(src, msg, h) -> (src, fromJust msg, h))
    & S.trace (\t -> if arg'.verbose then print t else pure ())
    & S.mapM (\(src, msg, h) -> do
                 res <- deidentify msg
                 pure (src, res.item, msg, h)
             )
    & S.filter (\(_, item, _, _) -> item /= Nothing)
    & S.map (\(src, item, msg, h) -> (src, fromJust item, msg, h))
    & S.map (\(src, item, msg, h) -> (src, item.value, msg, h))
    & S.filter (\(_, val, _, _) -> val /= Nothing)
    & S.map (\(src, val, msg, h) -> (src, fromJust val, msg, h))
    & S.trace (\(_, val, msg, _) -> if arg'.verbose then (print val >> print msg) else pure ())
    & S.drain

  where
    select :: A.Key -> Value -> Maybe Text
    select k o =  o ^? A.key k . _String
