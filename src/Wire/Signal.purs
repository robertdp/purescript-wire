module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

newtype Signal a
  = Signal
  { event :: Event a
  , modify :: (a -> a) -> Effect Unit
  , read :: Effect a
  , write :: a -> Effect Unit
  }

create :: forall a. a -> Effect { signal :: Signal a, cancel :: Effect Unit }
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    read' = Ref.read value

    write' a = modify' (const a)

    modify' f = Ref.modify f value >>= inner.push

    event' =
      Event.makeEvent \emit -> do
        Ref.read value >>= emit
        Event.subscribe inner.event emit

    signal = Signal { event: event', read: read', write: write', modify: modify' }
  pure { signal, cancel: inner.cancel }

event :: forall a. Signal a -> Event a
event (Signal s) = s.event

read :: forall a. Signal a -> Effect a
read (Signal s) = s.read

write :: forall a. a -> Signal a -> Effect Unit
write a (Signal s) = s.write a

modify :: forall a. (a -> a) -> Signal a -> Effect Unit
modify f (Signal s) = s.modify f

distinct :: forall a. Eq a => Signal a -> Signal a
distinct (Signal s) = Signal s { event = Event.distinct s.event }
