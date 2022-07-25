module Main where

import Model.CloudRun (app)
import Network.Wai.Handler.Warp
import System.IO (hPutStrLn, stderr)
import System.ReadEnvVar

main :: IO ()
main = do
  port <- readEnvDef "PORT" 8080
  runSettings (settings port) app
  where settings p = setPort p
                     $ setBeforeMainLoop (hPutStrLn stderr ("listening on port=" <> show p))
                                         defaultSettings
