module Gcp.Send where

import Control.Lens
import Gogol
import Gogol.Prelude
import System.IO (stderr)

sendGcp :: AllowRequest a scopes => a -> Proxy scopes -> IO (Rs a)
sendGcp r s = do
  l <- infoLogger
  sendGcpWithLogger l r s

sendGcpWithLogger :: AllowRequest a scopes => Logger -> a -> Proxy scopes -> IO (Rs a)
sendGcpWithLogger l r s = do
  env <- newEnv <&> (envLogger .~ l) . (envScopes .~ s)
  runResourceT $ send env r

infoLogger :: IO Logger
infoLogger = newLogger Info stderr

debugLogger :: IO Logger
debugLogger = newLogger Debug stderr

-- scope :: Proxy '["https://www.googleapis.com/auth/spreadsheets"]
-- scope = Proxy
