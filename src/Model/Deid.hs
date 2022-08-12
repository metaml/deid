module Model.Deid where

import Control.Lens
import Data.Aeson
import Data.Text
import Data.Time.Clock
import Database.Bloodhound hiding (key)
import GHC.Generics

data Deid = Deid { _dId :: DocId
                 , _lpOwner :: Text
                 , _serviceName :: Text
                 , _message :: Text
                 , _timestamp :: UTCTime
                 }
          deriving (Generic, Show, FromJSON, ToJSON)

makeLenses ''Deid
