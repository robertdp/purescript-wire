module Wire.Store.Atom.Pure where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.Store.Atom.Class (class Atom)

newtype Pure value
  = Pure
  { key :: String
  , default :: value
  , initialised :: Ref Boolean
  }

create ::
  forall value.
  { default :: value
  , key :: String
  } ->
  Effect (Pure value)
create { key, default } = do
  initialised <- Ref.new false
  pure $ Pure { key, default, initialised }

unsafeCreate ::
  forall value.
  { default :: value
  , key :: String
  } ->
  Pure value
unsafeCreate = unsafePerformEffect <<< create

instance atomPure :: Atom Pure where
  toStoreKey (Pure atom) = atom.key
  defaultValue (Pure atom) = atom.default
  isInitialised (Pure atom) = Ref.read atom.initialised
  initialise (Pure atom) _ = Ref.write true atom.initialised
  resetValue (Pure atom) signal = signal.write atom.default
  updateValue _ value signal = signal.write value
