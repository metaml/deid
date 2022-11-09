module Main where

import Database.Bloodhound
import qualified Etc.Delete as Cli

main :: IO ()
main = do
  arg <- Cli.args
  print arg
