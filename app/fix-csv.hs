module Main where

import Data.Csv
import Data.ByteString as B
import Data.Csv as Csv
import Model.Csv
import Model.Deid
import Prelude as P hiding (getLine)
import Streamly.Data.Stream as S
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  undefined
