module Model.PubSub where

import Data.Text
import Data.Text.Encoding
import Gcp.Send (sendGcp)
import Gogol.Data.Time
import Gogol.Prelude
import Gogol.PubSub

type AckId = Text
type MaxMessages = Int32
type Sub = Text

pull :: Sub -> MaxMessages -> IO (Rs PubSubProjectsSubscriptionsPull)
pull s max' = do
  let pr = newPullRequest { maxMessages = Just max' } :: PullRequest
      r = newPubSubProjectsSubscriptionsPull pr s
      p = Proxy :: Proxy '[Pubsub'FullControl]
  sendGcp r p

ack :: Sub -> [AckId] -> IO (Rs PubSubProjectsSubscriptionsAcknowledge)
ack sn is = do
  let ar = newAcknowledgeRequest { ackIds = Just is} :: AcknowledgeRequest
      r = newPubSubProjectsSubscriptionsAcknowledge ar sn
      p = Proxy :: Proxy '[Pubsub'FullControl]
  sendGcp r p

subscription :: Text -> Text -> Text
subscription p s = intercalate "/" ["projects", p, "subscriptions", s]

messages :: PullResponse -> Maybe [ReceivedMessage]
messages r = r.receivedMessages

toIdMsgPair :: ReceivedMessage -> (Maybe AckId, Maybe PubsubMessage)
toIdMsgPair r = (r.ackId, r.message)

toRow :: AckId -> PubsubMessage -> (AckId, Maybe Base64, Maybe DateTime)
toRow i msg = (i, msg.data', msg.publishTime)

toRow' :: AckId -> Base64 -> DateTime -> (AckId, Text, UTCTime)
toRow' i b64 dt = (i, decodeUtf8 b64.fromBase64, dt.unDateTime)
