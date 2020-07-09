module Wire.Signal where

import Prelude
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

newtype Signal a
  = Signal
  { event :: Event a
  , read :: Effect a
  }

create :: forall a. a -> Effect { signal :: Signal a, modify :: (a -> a) -> Effect Unit }
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    read' = Ref.read value

    event' =
      Event.makeEvent \notify -> do
        unsubscribe <- Event.subscribe inner.event notify
        read' >>= notify
        pure unsubscribe

    modify f = Ref.modify f value >>= inner.push
  pure { signal: Signal { event: event', read: read' }, modify }

subscribe :: forall b a. Signal a -> (a -> Effect b) -> Effect (Effect Unit)
subscribe (Signal s) notify = Event.subscribe s.event notify

distinct :: forall a. Eq a => Signal a -> Signal a
distinct (Signal s) = Signal s { event = Event.distinct s.event }

event :: Signal ~> Event
event (Signal s) = s.event

read :: Signal ~> Effect
read (Signal s) = s.read

derive instance functorSignal :: Functor Signal

instance applySignal :: Apply Signal where
  apply = ap

instance applicativeSignal :: Applicative Signal where
  pure a = Signal { event: pure a, read: pure a }

instance bindSignal :: Bind Signal where
  bind (Signal s) f = Signal { event: s.event >>= f >>> event, read: s.read >>= f >>> read }

instance monadSignal :: Monad Signal

instance monadRecSignal ∷ MonadRec Signal where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b
