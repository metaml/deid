module Model.CloudRun where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Optics
import Data.Text
import Data.Time.Clock
import Optics
import Servant
import qualified Model.BigQuery as BQ

type Api = ReqBody '[JSON] Object :> Post '[JSON] [Text]
           :<|> "ping"    :> Get '[PlainText] Text
           :<|> "version" :> Get '[PlainText] Text

app :: Application
app = let server = root
                   :<|> ping
                   :<|> version
      in serve (Proxy :: Proxy Api) server

root :: Object -> Handler [Text]
root obj = case o of
           Just o' -> do
             r <- liftIO $ BQ.tableInsert did [o'] pid tid
             pure $ [ pack . show $ r ]
           Nothing  -> throwError $ err400 { errBody = encode obj }
  where o = obj ^. at "message" ^? _Just % _Object
        did = "qa"
        tid = "qa-logs"
        pid = "lpgprj-gss-p-ctrlog-gl-01"

ping :: Handler Text
ping = do
  t <- liftIO $ getCurrentTime
  pure $ (pack . show) t

version :: Handler Text
version = pure "0.1.0"
