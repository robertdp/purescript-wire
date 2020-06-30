module Wire.Store where

import Prelude
import Data.Maybe (fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons, class Lacks)
import Unsafe.Coerce (unsafeCoerce)
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Class
import Wire.Store.Atom.Types (AtomSignal)

newtype Store (atoms :: # Type)
  = Store { atoms :: Object Foreign }

empty :: Store ()
empty = Store { atoms: Object.empty }

insertAtom ::
  forall key value before after atom.
  Atom atom =>
  IsSymbol key =>
  Lacks key before =>
  Cons key value before after =>
  atom key value ->
  Store before ->
  Effect (Store after)
insertAtom atom (Store store) = do
  signal <- Class.create atom
  pure
    $ Store
        store
          { atoms =
            Object.insert (reflectSymbol (SProxy :: _ key))
              ((unsafeCoerce :: forall a. AtomSignal a -> Foreign) signal)
              store.atoms
          }

getAtom ::
  forall key value atoms r atom.
  Atom atom =>
  IsSymbol key =>
  Cons key value r atoms =>
  atom key value ->
  Store atoms ->
  AtomSignal value
getAtom _ (Store { atoms }) =
  (unsafeCoerce :: forall a. Foreign -> AtomSignal a)
    $ unsafePartial fromJust
    $ Object.lookup (reflectSymbol (SProxy :: _ key)) atoms

resetAtom ::
  forall key value atoms r atom.
  Atom atom =>
  IsSymbol key =>
  Cons key value r atoms =>
  atom key value ->
  Store atoms ->
  Effect Unit
resetAtom atom = getAtom atom >>> Class.reset atom

updateAtom ::
  forall key value atoms r atom.
  Atom atom =>
  IsSymbol key =>
  Cons key value r atoms =>
  atom key value ->
  value ->
  Store atoms ->
  Effect Unit
updateAtom atom value = getAtom atom >>> Class.update atom value
