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
import Streamly.Prelude as S
import System.IO (stderr)
import qualified Etc.PubSub2Csv as Cli

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
    & S.map messages
    & S.map (fromMaybe [])
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable -- stream of lists to stream of elements of lists
    & S.trace (increment msgCounter)
    & S.map toIdMsgPair
    & S.filter (\(aid, msg) -> isJust aid && isJust msg)
    & S.map (\(aid, msg) -> (fromJust aid, fromJust msg))
    & S.map (\(aid, msg) -> toRow aid msg)
    & S.filter (\(_, b64, t) -> isJust b64 && isJust t)
    & S.map (\(aid, b64, t) -> (aid, fromJust b64, fromJust t))
    & S.map (\(aid, b64, t) -> toRow' aid b64 t)
    & S.map (\(aid, l, t) -> encode [LogRow aid l t]) -- a line of CSV
    & S.mapM (stdout' . T.decodeUtf8 . toStrict)
    & S.drain

  pulls <- readIORef pullCounter
  msgs <- readIORef msgCounter

  S.fromList [pulls, msgs]
    & S.map (T.pack . show)
    & S.mapM (T.hPutStrLn stderr)
    & S.drain

  where
    stderr' verbose = if verbose then hPutStrLn stderr else (\_ -> pure ())
    stdout' = T.putStrLn
    -- NB: streaming hack, revisit later
    increment :: IORef Int -> a -> IO ()
    increment r _ = modifyIORef' r (+1)
