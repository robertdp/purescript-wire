module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Sync value
  = Sync
  { key :: String
  , default :: value
  , handler :: Action value -> Handler value
  , initialised :: Ref Boolean
  }

type Handler a
  = FreeT (AtomicF a) Effect Unit

create ::
  forall value.
  { default :: value
  , handler :: Action value -> FreeT (AtomicF value) Effect Unit
  , key :: String
  } ->
  Effect (Sync value)
create { key, default, handler } = do
  initialised <- Ref.new false
  pure $ Sync { key, initialised, default, handler }

unsafeCreate ::
  forall value.
  { default :: value
  , handler :: Action value -> FreeT (AtomicF value) Effect Unit
  , key :: String
  } ->
  Sync value
unsafeCreate = unsafePerformEffect <<< create

instance atomSync :: Atom Sync where
  toStoreKey (Sync atom) = atom.key
  defaultValue (Sync atom) = atom.default
  isInitialised (Sync atom) = Ref.read atom.initialised
  initialise (Sync atom) signal = do
    Ref.write true atom.initialised
    run (atom.handler Initialize) signal
  resetValue (Sync atom) signal = do
    signal.write atom.default
    run (atom.handler Initialize) signal
  updateValue (Sync atom) value = run (atom.handler (Update value))

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run = flip interpret
