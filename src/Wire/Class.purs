module Wire.Class where

import Prelude
import Effect (Effect)
import Wire.Event (Event, Subscribe, makeEvent, subscribe)

class EventSource source a | source -> a where
  source :: source -> Event a

class EventSink sink a | sink -> a where
  sink :: forall source. EventSource source a => sink -> source -> Effect (Effect Unit)

instance eventSourceEvent :: EventSource (Event a) a where
  source = identity

instance eventSourceSubscriber :: EventSource ((a -> Effect Unit) -> Effect (Effect Unit)) a where
  source = makeEvent

instance eventSinkSubscriber :: EventSink (a -> Effect Unit) a where
  sink to from = subscribe (source from) to

source_ :: forall a. Subscribe a -> Event a
source_ = source

sink_ :: forall source a. EventSource source a => (a -> Effect Unit) -> source -> Effect (Effect Unit)
sink_ = sink
