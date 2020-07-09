module Wire.React.Sync where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.React.Class (class Atom)

newtype Sync a
  = Sync
  { initial :: a
  , load :: Effect a
  , save :: a -> Effect Unit
  , signal :: Signal a
  , modify :: (a -> a) -> Effect Unit
  }

create ::
  forall a.
  { load :: Effect a
  , save :: a -> Effect Unit
  } ->
  Effect (Sync a)
create { load, save } = do
  initial <- load
  { signal, modify } <- Signal.create initial
  pure $ Sync { initial, load, save, signal, modify }

unsafeCreate ::
  forall a.
  { load :: Effect a
  , save :: a -> Effect Unit
  } ->
  Sync a
unsafeCreate = unsafePerformEffect <<< create

instance atomSync :: Atom Sync where
  default (Sync atom) = atom.initial
  read (Sync atom) = Signal.read atom.signal
  modify (Sync atom) f = do
    atom.modify f
    Signal.read atom.signal >>= atom.save
  reset (Sync atom) = do
    value <- atom.load
    atom.modify (const value)
  subscribe (Sync atom) = Signal.subscribe atom.signal
  signal (Sync atom) = atom.signal
