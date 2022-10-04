module Model.Csv where

import Data.Csv
import Data.Text
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Text.Time
import Data.Time.Clock
import GHC.Generics

data LogRow = LogRow { ackId :: Text
                     , payload :: Text
                     , timestamp :: UTCTime
                     }
            deriving (Eq, Generic, ToRecord, FromRecord, Show)

instance FromField UTCTime where
  parseField = pure . parseISODateTime . decodeUtf8

instance ToField UTCTime where
  toField = encodeUtf8 . toStrict . formatISODateTime
