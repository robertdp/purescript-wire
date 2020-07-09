module Wire.Store.Atom.Pure where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Wire.Event as Event
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)

newtype Pure a
  = Pure
  { initial :: a
  , signal :: Signal a
  }

create :: forall a. a -> Effect (Pure a)
create initial = do
  signal <- Signal.create initial
  pure $ Pure { initial, signal }

unsafeCreate :: forall a. a -> Pure a
unsafeCreate = unsafePerformEffect <<< create

instance atomPure :: Atom Pure where
  default (Pure atom) = atom.initial
  read (Pure atom) = atom.signal.read
  modify f (Pure atom) = atom.signal.modify f
  reset (Pure atom) = atom.signal.modify (const atom.initial)
  subscribe k (Pure atom) = Event.subscribe atom.signal.event k
  signal (Pure atom) = atom.signal
