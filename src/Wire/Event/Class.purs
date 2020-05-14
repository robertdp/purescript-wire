module Wire.Event.Class where

import Prelude
import Wire.Event (Event, Subscribe, makeEvent, subscribe)

class EventSource source a | source -> a where
  source :: source -> Event a

instance eventSourceEvent :: EventSource (Event a) a where
  source = identity

source_ :: forall a. Subscribe a -> Event a
source_ = makeEvent

sink :: forall source a. EventSource source a => source -> Subscribe a
sink from to = subscribe (source from) to
