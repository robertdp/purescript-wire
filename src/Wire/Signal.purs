module Wire.Signal where

import Prelude
import Control.Apply (lift2)
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event, Subscribe)
import Wire.Event as Event
import Wire.Event.Class (class EventSource, sink)

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  }

create :: forall a. a -> Effect { signal :: Signal a, write :: a -> Effect Unit }
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    write a = do
      Ref.write a value
      inner.push a

    event =
      Event.makeEvent \emit -> do
        _ <- Ref.read value >>= emit
        Event.subscribe inner.event emit

    signal = Signal { event, read: Ref.read value }
  pure { signal, write }

read :: forall a. Signal a -> Effect a
read (Signal s) = s.read

subscribe :: forall a. Signal a -> Subscribe a
subscribe = sink

distinct :: forall a. Eq a => Signal a -> Signal a
distinct (Signal s) = Signal s { event = Event.distinct s.event }

derive instance functorSignal :: Functor Signal

instance applySignal :: Apply Signal where
  apply (Signal f) (Signal a) =
    Signal
      { event: apply f.event a.event
      , read: apply f.read a.read
      }

instance applicativeSignal :: Applicative Signal where
  pure a = Signal { event: pure a, read: pure a }

instance bindSignal :: Bind Signal where
  bind (Signal s) f =
    Signal
      { event: s.event >>= \a -> case f a of Signal n -> n.event
      , read: s.read >>= \a -> case f a of Signal n -> n.read
      }

instance monadSignal :: Monad Signal

instance eventSourceSignal :: EventSource (Signal a) a where
  source (Signal s) = s.event

instance semigroupSignal :: Semigroup a => Semigroup (Signal a) where
  append = lift2 append

instance monoidSignal :: Monoid a => Monoid (Signal a) where
  mempty = Signal { event: mempty, read: mempty }
