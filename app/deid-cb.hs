module Main where

import Control.Monad (when)
import Data.Function ((&))
import Data.IORef
import Data.Maybe
import Data.Text as T
import Data.Text.IO as T
import Model.Deid
import Model.PubSub
import Prelude as P
import Streamly.Prelude as S
import System.IO (stderr)
import qualified Etc.DeidCb as Cli

main :: IO ()
main = do
  arg <- Cli.args

  when arg.verbose (print arg)

  pullCounter <- newIORef (0 :: Int)
  msgCounter  <- newIORef (0 :: Int)
  inspectCounter <- newIORef (0 :: Int)
  deidCounter <- newIORef (0 :: Int)

  T.putStrLn $ T.intercalate "," header'

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
    & S.filter (\(id', msg) -> isJust id' && isJust msg)
    & S.mapM print
    & S.drain

  pulls <- readIORef pullCounter
  msgs <- readIORef msgCounter
  inspections <- readIORef inspectCounter
  deids <- readIORef deidCounter

  print pulls >> print msgs >> print inspections >> print deids
  where
    stderr' verbose = if verbose then hPutStrLn stderr else (\_ -> pure ())
    stdout' = T.putStrLn
    -- NB: streaming hack, revisit later
    increment :: IORef Int -> a -> IO ()
    increment r _ = modifyIORef' r (+1)
