module Main where

import Data.Function ((&))
import Data.IORef
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
  arg' <- Cli.arg

  hitCounter  <- newIORef (0 :: Int)
  inspectCounter <- newIORef (0 :: Int)
  deidCounter <- newIORef (0 :: Int)

  T.putStrLn $ T.intercalate "," header'

  S.fromList arg'.subs
    & S.trace (\s -> if arg'.verbose then (hPutStrLn stderr s) else pure ())
    & S.mapM (\s -> pull s arg'.maxResults)
    & S.mapM print
    & S.drain

  hits <- readIORef hitCounter
  inspections <- readIORef inspectCounter
  deids <- readIORef deidCounter

  print hits >> print inspections >> print deids

  pure ()
