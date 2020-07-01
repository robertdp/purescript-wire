module Wire.Store.Atom.Pure (Pure, create, unsafeCreate) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.Store.Atom.Class (class Atom)

newtype Pure value
  = Pure
  { key :: String
  , initial :: value
  , initialised :: Ref Boolean
  }

create ::
  forall value.
  { initial :: value
  , key :: String
  } ->
  Effect (Pure value)
create { key, initial } = do
  initialised <- Ref.new false
  pure $ Pure { key, initial, initialised }

unsafeCreate ::
  forall value.
  { initial :: value
  , key :: String
  } ->
  Pure value
unsafeCreate = unsafePerformEffect <<< create

instance atomPure :: Atom Pure where
  storeKey (Pure atom) = atom.key
  initialValue (Pure atom) = atom.initial
  isInitialised (Pure atom) = Ref.read atom.initialised
  initialise (Pure atom) _ = Ref.write true atom.initialised
  reset (Pure atom) signal = signal.write atom.initial
  update _ value signal = signal.write value
