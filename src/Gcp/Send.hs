module Gcp.Send where

import Control.Lens
import Gogol
import Gogol.Prelude
import System.IO (stdout)

scope :: Proxy '["https://www.googleapis.com/auth/spreadsheets"]
scope = Proxy

send' :: AllowRequest a scopes => a -> Proxy scopes -> IO (Rs a)
send' r s = do
  log' <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ log') . (envScopes .~ s)
  runResourceT $ send env r

-- r = newBigQueryTablesList "foo" "bar"
-- send' r (Proxy :: Proxy '[Bigquery'FullControl])
-- s :: Proxy '[Bigquery'FullControl] = Proxy
-- send' r s
