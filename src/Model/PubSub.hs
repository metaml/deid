module Model.PubSub where

import Data.Text
import Gcp.Send (sendGcp)
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

toTuple :: AckId -> PubsubMessage -> (AckId, Maybe PubsubMessage_Attributes, Maybe Base64, Maybe DateTime)
toTuple i msg = (i, msg.attributes, msg.data', msg.publishTime)
