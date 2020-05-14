module Wire.Signal where

import Prelude
import Control.Apply (lift2)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event, Subscribe)
import Wire.Event as Event
import Wire.Event.Class (class EventSource, sink, source)

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

share :: forall a. Signal a -> Effect (Signal a)
share source = do
  subscriberCount <- Ref.new 0
  cancelSource <- Ref.new Nothing
  { signal: Signal shared, write } <- create =<< read source
  let
    incrementCount = do
      count <- Ref.modify (add 1) subscriberCount
      when (count == 1) do
        cancel <- subscribe source write
        Ref.write (Just cancel) cancelSource

    decrementCount = do
      count <- Ref.modify (sub 1) subscriberCount
      when (count == 0) do
        Ref.read cancelSource >>= sequence_
        Ref.write Nothing cancelSource

    event =
      Event.makeEvent \emit -> do
        incrementCount
        cancel <- Event.subscribe shared.event emit
        pure do cancel *> decrementCount
  pure $ Signal { event, read: shared.read }

sample :: forall a. a -> Event a -> Effect { signal :: Signal a, unsubscribe :: Effect Unit }
sample init event = do
  { signal, write } <- create init
  unsubscribe <- Event.subscribe event write
  pure { signal, unsubscribe }

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
      { event: s.event >>= f >>> source
      , read: s.read >>= f >>> read
      }

instance monadSignal :: Monad Signal

instance eventSourceSignal :: EventSource (Signal a) a where
  source (Signal s) = s.event

instance semigroupSignal :: Semigroup a => Semigroup (Signal a) where
  append = lift2 append

instance monoidSignal :: Monoid a => Monoid (Signal a) where
  mempty = Signal { event: mempty, read: mempty }
