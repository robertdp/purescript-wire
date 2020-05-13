module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Class (class EventSource, source)
import Wire.Event (Event, Subscriber)
import Wire.Event as Event

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  , kill :: Effect Unit
  }

fromSource :: forall source a. EventSource source a => a -> source -> Effect (Signal a)
fromSource init from = do
  value <- Ref.new init
  shared <- Event.share (source from)
  kill <- Event.subscribe shared.event (flip Ref.write value)
  pure $ Signal { event: shared.event, read: Ref.read value, kill }

read :: forall a. Signal a -> Effect a
read (Signal s) = s.read

subscribe :: forall a. Signal a -> Subscriber a
subscribe (Signal s) k = do
  _ <- s.read >>= k
  Event.subscribe s.event k

derive instance functorSignal :: Functor Signal
