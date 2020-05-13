module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event, Canceler)
import Wire.Event as Event
import Wire.Event.Class (class EventSink, class EventSource, sink, source, source_)

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  , push :: a -> Effect Unit
  , kill :: Effect Unit
  }

create :: forall source a. EventSource source a => a -> source -> Effect (Signal a)
create init from = do
  value <- Ref.new init
  { event, push } <- Event.create
  cancel <-
    Event.subscribe (source from) \a -> do
      Ref.write a value
      push a
  pure $ Signal { event: event, read: Ref.read value, push, kill: cancel }

read :: forall a. Signal a -> Effect a
read (Signal s) = s.read

subscribe :: forall sink a. EventSink sink a => Signal a -> sink -> Effect (Canceler)
subscribe s k = do
  _ <- sink (source_ \emit -> read s >>= emit *> mempty) k
  sink s k

write :: forall a. a -> Signal a -> Effect Unit
write a (Signal s) = s.push a

kill :: forall a. Signal a -> Effect Unit
kill (Signal s) = s.kill

instance eventSourceSignal :: EventSource (Signal a) a where
  source (Signal s) = s.event

instance eventSinkSignal :: EventSink (Signal a) a where
  sink from (Signal s) = Event.subscribe (source from) s.push
