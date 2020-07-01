module Wire.Store where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Class
import Wire.Store.Atom.Types (AtomSignal)

newtype Store
  = Store { atoms :: Ref (Object Foreign) }

create :: Effect Store
create = do
  atoms <- Ref.new Object.empty
  pure $ Store { atoms }

toForeign :: forall atom value. Atom atom => atom value -> AtomSignal value -> Foreign
toForeign _ = unsafeCoerce

fromForeign :: forall atom value. Atom atom => atom value -> Foreign -> AtomSignal value
fromForeign _ = unsafeCoerce

lookup :: forall value atom. Atom atom => atom value -> Store -> Effect (Maybe (AtomSignal value))
lookup atom (Store store) = do
  let
    storeKey = Class.toStoreKey atom
  storedSignal <- map (fromForeign atom) <<< Object.lookup storeKey <$> Ref.read store.atoms
  isInitialised <- Class.isInitialised atom
  case storedSignal, isInitialised of
    Just signal, true -> do
      pure $ pure signal
    Nothing, false -> do
      signal <- Signal.create $ Class.defaultValue atom
      Ref.modify_ (Object.insert storeKey (toForeign atom signal)) store.atoms
      Class.initialise atom signal
      pure $ pure signal
    Nothing, true -> do
      Console.warn $ "The atom " <> show storeKey <> " has been initialised but cannot be found in the store."
      pure Nothing
    Just _, false -> do
      Console.warn $ "A different atom with the key " <> show storeKey <> " is already in the store."
      pure Nothing

unsafeLookup :: forall atom value. Atom atom => atom value -> Store -> Maybe (AtomSignal value)
unsafeLookup atom store = unsafePerformEffect $ lookup atom store

reset :: forall atom value. Atom atom => atom value -> Store -> Effect Unit
reset atom store = lookup atom store >>= traverse_ (Class.resetValue atom)

update :: forall atom value. Atom atom => atom value -> value -> Store -> Effect Unit
update atom value store = lookup atom store >>= traverse_ (Class.updateValue atom value)
