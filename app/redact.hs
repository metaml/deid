module Main where

import Control.Lens ((^?))
import Data.Aeson.Key as A
import Data.Aeson.Lens as A
import Data.Aeson.Types (Value, emptyObject)
import Data.Either
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text as T
import Database.Bloodhound hiding (key)
import Gogol.DLP.Types
import Model.Deid
import Model.Elastic as Es
import Prelude as P
import Streamly.Data.Stream as S
import qualified Etc.Redact as Cli
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as U

main :: IO ()
main = do
  arg' <- Cli.arg
  let esUrl = Es.server arg'.server arg'.port

  S.fromList [(arg'.index, arg'.docId)]
    & fmap (\(i, d) -> (IndexName i, DocId d))
    & S.trace (\t -> if arg'.verbose then print t else pure ())
    & S.mapM (\(i, d) -> document esUrl i d)
    & fmap rights
    & S.filter (not . P.null)
    & S.unfoldMany U.fromList
    & fmap searchHits
    & fmap hits
    & S.unfoldMany U.fromList    
    & S.trace (\h -> if arg'.verbose then print h else pure ())
    & fmap (\h -> (hitSource h, h))
    & fmap (\(src, h) -> (fromMaybe emptyObject src, h))
    & fmap (\(src, h) -> (src, select "message" src, h))
    & S.filter (\(_, msg, _) -> msg /= Nothing)
    & fmap (\(src, msg, h) -> (src, fromJust msg, h))
    & S.trace (\t -> if arg'.verbose then print t else pure ())
    & S.mapM (\(src, msg, h) -> do
                 res <- deidentify msg
                 pure (src, res.item, msg, h)
             )
    & S.filter (\(_, item, _, _) -> item /= Nothing)
    & fmap (\(src, item, msg, h) -> (src, fromJust item, msg, h))
    & fmap (\(src, item, msg, h) -> (src, item.value, msg, h))
    & S.filter (\(_, val, _, _) -> val /= Nothing)
    & fmap (\(src, val, msg, h) -> (src, fromJust val, msg, h))
    & S.trace (\(_, val, msg, _) -> if arg'.verbose then (print val >> print msg) else pure ())
    & S.fold F.drain

  where
    select :: A.Key -> Value -> Maybe Text
    select k o =  o ^? A.key k . _String
