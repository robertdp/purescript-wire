module Wire.Store.Atom.Class where

import Prelude
import Effect (Effect)
import Wire.Store.Atom.Types (AtomSignal)

class Atom (atom :: Type -> Type) where
  toStoreKey :: forall value. atom value -> String
  defaultValue :: forall value. atom value -> value
  isInitialised :: forall value. atom value -> Effect Boolean
  initialise :: forall value. atom value -> AtomSignal value -> Effect Unit
  resetValue :: forall value. atom value -> AtomSignal value -> Effect Unit
  updateValue :: forall value. atom value -> value -> AtomSignal value -> Effect Unit
