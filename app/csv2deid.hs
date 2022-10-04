module Main where

import Control.Monad (when)
import Control.Lens
import Data.ByteString (getLine)
import Data.Csv
import Data.IORef
import Data.Text as T
import Model.Csv
import Prelude as P hiding (getLine)
import Streamly.Prelude as S
import System.IO (stderr)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Vector as V
import qualified Etc.Csv2Deid as Cli

main :: IO ()
main = do
  arg <- Cli.args
  when arg.verbose (print arg)

  pullCounter <- newIORef (0 :: Int)
  msgCounter  <- newIORef (0 :: Int)
  inspectCounter <- newIORef (0 :: Int)
  deidCounter <- newIORef (0 :: Int)

  S.repeatM getLine
    & S.map C.fromStrict
    & S.mapM (\l -> case (decode NoHeader l :: Either String (V.Vector LogRow)) of
                      Left e  -> print e >> pure []
                      Right v -> pure $ V.toList v
             )
    & S.filter (not . P.null)
    & S.concatMap S.fromFoldable
    & S.mapM print
    & S.drain

  pulls <- readIORef pullCounter
  msgs <- readIORef msgCounter
  inspections <- readIORef inspectCounter
  deids <- readIORef deidCounter

  print pulls >> print msgs >> print inspections >> print deids
  where
    increment :: IORef Int -> a -> IO ()
    increment r _ = modifyIORef' r (+1)
