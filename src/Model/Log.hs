module Model.Log where

import Data.Aeson
import Data.Text
import Data.Time.Clock
import GHC.Generics

type Message = Text

data Log = Log { timestamp :: UTCTime
               , message :: Text
               }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON)

mkLog :: Message -> IO Log
mkLog msg  = do
  t <- getCurrentTime
  pure $ Log t msg
