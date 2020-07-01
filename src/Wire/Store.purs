module Wire.Store (Store, context, create, lookup, unsafeLookup, reset, update) where

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
import React.Basic (ReactContext)
import React.Basic as React
import Unsafe.Coerce (unsafeCoerce)
import Wire.Signal as Signal
import Wire.Store.Atom (AtomSignal)
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Class

newtype Store
  = Store { atoms :: Ref (Object (AtomSignal Foreign)) }

context :: ReactContext Store
context = unsafePerformEffect $ React.createContext =<< create

create :: Effect Store
create = do
  atoms <- Ref.new Object.empty
  pure $ Store { atoms }

toForeign :: forall atom value. Atom atom => atom value -> AtomSignal value -> AtomSignal Foreign
toForeign _ = unsafeCoerce

fromForeign :: forall atom value. Atom atom => atom value -> AtomSignal Foreign -> AtomSignal value
fromForeign _ = unsafeCoerce

lookup :: forall value atom. Atom atom => atom value -> Store -> Effect (Maybe (AtomSignal value))
lookup atom (Store store) = do
  let
    storeKey = Class.storeKey atom
  storedSignal <- map (fromForeign atom) <<< Object.lookup storeKey <$> Ref.read store.atoms
  isInitialised <- Class.isInitialised atom
  case storedSignal, isInitialised of
    Just signal, true -> do
      pure $ pure signal
    Nothing, false -> do
      signal <- Signal.create $ Class.initialValue atom
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
reset atom store = lookup atom store >>= traverse_ (Class.reset atom)

update :: forall atom value. Atom atom => atom value -> Store -> value -> Effect Unit
update atom store value = lookup atom store >>= traverse_ (Class.update atom value)
