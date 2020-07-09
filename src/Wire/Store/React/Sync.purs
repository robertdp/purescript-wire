module Wire.React.Sync where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Wire.Event as Event
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.React.Class (class Atom)

newtype Sync a
  = Sync
  { initial :: a
  , load :: Effect a
  , save :: a -> Effect Unit
  , signal :: Signal a
  }

create ::
  forall a.
  { load :: Effect a
  , save :: a -> Effect Unit
  } ->
  Effect (Sync a)
create { load, save } = do
  initial <- load
  signal <- Signal.create initial
  pure $ Sync { initial, load, save, signal }

unsafeCreate ::
  forall a.
  { load :: Effect a
  , save :: a -> Effect Unit
  } ->
  Sync a
unsafeCreate = unsafePerformEffect <<< create

instance atomSync :: Atom Sync where
  default (Sync atom) = atom.initial
  read (Sync atom) = atom.signal.read
  modify f (Sync atom) = do
    atom.signal.modify f
    atom.signal.read >>= atom.save
  reset (Sync atom) = do
    value <- atom.load
    atom.signal.modify (const value)
  subscribe notify (Sync atom) = Event.subscribe atom.signal.event notify
  signal (Sync atom) = atom.signal
