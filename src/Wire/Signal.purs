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

create ::
  forall a.
  a ->
  Effect
    { signal :: Signal a
    , write :: a -> Effect Unit
    , modify :: (a -> a) -> Effect Unit
    }
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    read' = Ref.read value

    event =
      Event.makeEvent \notify -> do
        read' >>= notify
        Event.subscribe inner.event notify

    modify' f = Ref.modify f value >>= inner.push

    signal =
      Signal
        { event
        , read: read'
        }
  pure
    { signal
    , write: modify' <<< const
    , modify: modify'
    }

distinct :: forall a. Eq a => Signal a -> Signal a
distinct (Signal s) = Signal s { event = Event.distinct s.event }

createDistinct ::
  forall a.
  Eq a =>
  a ->
  Effect
    { signal :: Signal a
    , write :: a -> Effect Unit
    , modify :: (a -> a) -> Effect Unit
    }
createDistinct init = do
  signal <- create init
  pure $ signal { signal = distinct signal.signal }

subscribe :: forall a. Signal a -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribe (Signal s) notify = Event.subscribe s.event notify

toEvent :: Signal ~> Event
toEvent (Signal s) = s.event

read :: Signal ~> Effect
read (Signal s) = s.read

instance functorSignal :: Functor Signal where
  map f (Signal s) = Signal { event: map f s.event, read: map f s.read }

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
      { event: s.event >>= f >>> toEvent
      , read: s.read >>= f >>> read
      }

instance monadSignal :: Monad Signal

instance monadRecSignal ∷ MonadRec Signal where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b
