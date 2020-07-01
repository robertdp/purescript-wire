module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Async value
  = Async
  { key :: String
  , initial :: value
  , handler :: Action value -> Handler value
  , initialised :: Ref Boolean
  }

create ::
  forall value.
  { initial :: value
  , handler :: Action value -> FreeT (AtomicF value) Aff Unit
  , key :: String
  } ->
  Effect (Async value)
create { key, initial, handler } = do
  initialised <- Ref.new false
  pure $ Async { key, initialised, initial, handler }

unsafeCreate ::
  forall value.
  { initial :: value
  , handler :: Action value -> FreeT (AtomicF value) Aff Unit
  , key :: String
  } ->
  Async value
unsafeCreate = unsafePerformEffect <<< create

type Handler a
  = FreeT (AtomicF a) Aff Unit

instance atomAsync :: Atom Async where
  storeKey (Async atom) = atom.key
  initialValue (Async atom) = atom.initial
  isInitialised (Async atom) = Ref.read atom.initialised
  initialise (Async atom) signal = do
    Ref.write true atom.initialised
    run (atom.handler Initialize) signal
  reset (Async atom) signal = do
    signal.write atom.initial
    run (atom.handler Initialize) signal
  update (Async atom) value = run (atom.handler (Update value))

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run handler signal = launchAff_ $ interpret signal handler
