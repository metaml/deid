module Gcp.Send where

import Control.Lens
import Gogol
import Gogol.Prelude
import System.IO (stderr)

scope :: Proxy '["https://www.googleapis.com/auth/spreadsheets"]
scope = Proxy

send' :: AllowRequest a scopes => a -> Proxy scopes -> IO (Rs a)
send' r s = do
  log' <- newLogger Info stderr
  env <- newEnv <&> (envLogger .~ log') . (envScopes .~ s)
  runResourceT $ send env r

sendLog :: AllowRequest a scopes => Logger -> a -> Proxy scopes -> IO (Rs a)
sendLog l r s = do
  env <- newEnv <&> (envLogger .~ l) . (envScopes .~ s)
  runResourceT $ send env r
