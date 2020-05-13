module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Class (class EventSink, class EventSource, sink, source)
import Wire.Event (Event, Canceler)
import Wire.Event as Event

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  , push :: a -> Effect Unit
  , kill :: Effect Unit
  }

signal :: forall source a. EventSource source a => a -> source -> Effect (Signal a)
signal init from = do
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
subscribe (Signal s) k = do
  value <- s.read
  _ <- sink k (source (pure value :: Event _))
  sink k (source (Signal s))

write :: forall a. a -> Signal a -> Effect Unit
write a (Signal s) = s.push a

kill :: forall a. Signal a -> Effect Unit
kill (Signal s) = s.kill

instance eventSourceSignal :: EventSource (Signal a) a where
  source (Signal s) = s.event

instance eventSinkSignal :: EventSink (Signal a) a where
  sink (Signal s) from = Event.subscribe (source from) s.push
