module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event, Subscribe)
import Wire.Event as Event
import Wire.Event.Class (class EventSource, sink, source)

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
  pure
    $ Signal
        { event:
            Event.makeEvent \emit -> do
              _ <- Ref.read value >>= emit
              Event.subscribe event emit
        , read: Ref.read value
        , push
        , kill: cancel
        }

read :: forall a. Signal a -> Effect a
read (Signal s) = s.read

subscribe :: forall a. Signal a -> Subscribe a
subscribe = sink

write :: forall a. Signal a -> a -> Effect Unit
write (Signal s) = s.push

kill :: forall a. Signal a -> Effect Unit
kill (Signal s) = s.kill

instance eventSourceSignal :: EventSource (Signal a) a where
  source (Signal s) = s.event
