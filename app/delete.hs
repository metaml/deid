module Main where

import Database.Bloodhound
import Model.Elastic
import Network.HTTP.Client (defaultManagerSettings)
import qualified Etc.Delete as Cli

main :: IO ()
main = do
  arg <- Cli.args
  BHResponse r <- withBH defaultManagerSettings (server arg.server arg.port)
                  $ deleteDocument (IndexName arg.index) (DocId arg.docId)
  print r
