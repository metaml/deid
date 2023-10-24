module Main where

import Control.Monad (when)
import Control.Lens
import Data.Csv
import Data.IORef
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.IO as T
import Data.ByteString.Lazy (toStrict)
import Model.Csv
import Model.PubSub
import Prelude as P
import Streamly.Data.Stream as S
import System.IO (stderr)
import qualified Etc.PubSub2Csv as Cli
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as U

main :: IO ()
main = do
  arg <- Cli.args

  when arg.verbose (print arg)

  pullCounter <- newIORef (0 :: Int)
  msgCounter  <- newIORef (0 :: Int)

  S.fromList arg.subs
    & S.trace (stderr' arg.verbose)
    & S.mapM (flip pull arg.maxResults)
    & S.trace (increment pullCounter)
    & fmap messages
    & fmap (fromMaybe [])
    & S.filter (not . P.null)
    & S.unfoldMany U.fromList    
    & S.trace (increment msgCounter)
    & fmap toIdMsgPair
    & S.filter (\(aid, msg) -> isJust aid && isJust msg)
    & fmap (\(aid, msg) -> (fromJust aid, fromJust msg))
    & fmap (\(aid, msg) -> toRow aid msg)
    & S.filter (\(_, b64, t) -> isJust b64 && isJust t)
    & fmap (\(aid, b64, t) -> (aid, fromJust b64, fromJust t))
    & fmap (\(aid, b64, t) -> toRow' aid b64 t)
    & fmap (\(aid, l, t) -> encode [LogRow aid l t]) -- a line of CSV
    & S.mapM (stdout' . T.decodeUtf8 . toStrict)
    & S.fold F.drain

  pulls <- readIORef pullCounter
  msgs <- readIORef msgCounter

  S.fromList [pulls, msgs]
    & fmap (T.pack . show)
    & S.mapM (T.hPutStrLn stderr)
    & S.fold F.drain

  where
    stderr' verbose = if verbose then hPutStrLn stderr else (\_ -> pure ())
    stdout' = T.putStrLn
    -- NB: streaming hack, revisit later
    increment :: IORef Int -> a -> IO ()
    increment r _ = modifyIORef' r (+1)
