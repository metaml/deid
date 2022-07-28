module Main where

import Data.Function ((&))
import Model.Elastic
import Streamly.Prelude as S

main :: IO ()
main = do
  let s = server "localhost" 9200
  ixs <- currentIndexes s
  S.fromList ixs
    & S.mapM (\i -> print i >> pure i)
    & S.mapM (documents s)
    & S.mapM print
    & S.drain
