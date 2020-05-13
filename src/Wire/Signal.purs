module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event, Subscribe)
import Wire.Event as Event
import Wire.Event.Class (class EventSource, sink)

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  , write :: a -> Effect Unit
  }

create :: forall a. a -> Effect (Signal a)
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    push a = do
      Ref.write a value
      inner.push a

    event =
      Event.makeEvent \emit -> do
        _ <- Ref.read value >>= emit
        Event.subscribe inner.event emit
  pure
    $ Signal
        { event
        , read: Ref.read value
        , write: push
        }

read :: forall a. Signal a -> Effect a
read (Signal s) = s.read

subscribe :: forall a. Signal a -> Subscribe a
subscribe = sink

write :: forall a. Signal a -> a -> Effect Unit
write (Signal s) = s.write

instance eventSourceSignal :: EventSource (Signal a) a where
  source (Signal s) = s.event
