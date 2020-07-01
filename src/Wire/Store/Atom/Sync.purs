module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Sync value
  = Sync
  { key :: String
  , initial :: value
  , handler :: Action value -> Handler value
  , initialised :: Ref Boolean
  }

type Handler a
  = FreeT (AtomicF a) Effect Unit

create ::
  forall value.
  { initial :: value
  , handler :: Action value -> FreeT (AtomicF value) Effect Unit
  , key :: String
  } ->
  Effect (Sync value)
create { key, initial, handler } = do
  initialised <- Ref.new false
  pure $ Sync { key, initialised, initial, handler }

unsafeCreate ::
  forall value.
  { initial :: value
  , handler :: Action value -> FreeT (AtomicF value) Effect Unit
  , key :: String
  } ->
  Sync value
unsafeCreate = unsafePerformEffect <<< create

instance atomSync :: Atom Sync where
  storeKey (Sync atom) = atom.key
  initialValue (Sync atom) = atom.initial
  isInitialised (Sync atom) = Ref.read atom.initialised
  initialise (Sync atom) signal = do
    Ref.write true atom.initialised
    run (atom.handler Initialize) signal
  reset (Sync atom) signal = do
    signal.write atom.initial
    run (atom.handler Initialize) signal
  update (Sync atom) value = run (atom.handler (Update value))

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run = flip interpret
