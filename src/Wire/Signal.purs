module Wire.Signal where

import Prelude
import Control.Plus (empty)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Wire.Event (Canceler, Event, Subscriber)
import Wire.Event as Event

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  }

create ::
  forall a.
  a ->
  Effect
    { signal :: Signal a
    , write :: a -> Effect Unit
    , modify :: (a -> a) -> Effect Unit
    , cancel :: Effect Unit
    }
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    modify' f = Ref.modify f value >>= Aff.launchAff_ <<< inner.push

    signal =
      Signal
        { event: inner.event
        , read: Ref.read value
        }
  pure
    { signal
    , write: modify' <<< const
    , modify: modify'
    , cancel: inner.cancel
    }

distinct :: forall a. Eq a => Signal a -> Signal a
distinct (Signal s) = Signal s { event = Event.distinct s.event }

createDistinct ::
  forall a.
  Eq a =>
  a ->
  Effect
    { cancel :: Effect Unit
    , modify :: (a -> a) -> Effect Unit
    , signal :: Signal a
    , write :: a -> Effect Unit
    }
createDistinct init = do
  signal <- create init
  pure $ signal { signal = distinct signal.signal }

subscribe :: forall a. Signal a -> Subscriber a -> Effect Canceler
subscribe (Signal s) k = do
  s.read >>= Aff.launchAff_ <<< k
  Event.subscribe s.event k

event :: Signal ~> Event
event (Signal s) = s.event

read :: Signal ~> Effect
read (Signal s) = s.read

instance functorSignal :: Functor Signal where
  map f (Signal s) = Signal { event: map f s.event, read: map f s.read }

instance applySignal :: Apply Signal where
  apply (Signal f) (Signal a) = Signal { event: apply f.event a.event, read: apply f.read a.read }

instance applicativeSignal :: Applicative Signal where
  pure a = Signal { event: empty, read: pure a }

instance bindSignal :: Bind Signal where
  bind (Signal s) f = Signal { event: s.event >>= f >>> event, read: s.read >>= f >>> read }

instance monadSignal :: Monad Signal
