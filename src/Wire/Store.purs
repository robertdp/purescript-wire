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
import Wire.Store.Atom (Atom(..))
import Wire.Store.Atom.Async as Async
import Wire.Store.Atom.Pure as Pure
import Wire.Store.Atom.Sync as Sync
import Wire.Store.Atom.Types (AtomSignal)

newtype Store (atoms :: # Type)
  = Store { atoms :: Object Foreign }

empty :: Store ()
empty = Store { atoms: Object.empty }

withAtom ::
  forall key value before after.
  IsSymbol key =>
  Lacks key before =>
  Cons key value before after =>
  Atom key value ->
  Store before ->
  Effect (Store after)
withAtom atom (Store store) = do
  signal <- case atom of
    Async a -> Async.createSignal a
    Sync a -> Sync.createSignal a
    Pure a -> Pure.createSignal a
  pure
    $ Store
        store
          { atoms =
            Object.insert (reflectSymbol (SProxy :: _ key))
              ((unsafeCoerce :: forall a. AtomSignal a -> Foreign) signal)
              store.atoms
          }

getAtomSignal ::
  forall key value atoms r.
  IsSymbol key =>
  Cons key value r atoms =>
  Atom key value ->
  Store atoms -> AtomSignal value
getAtomSignal _ (Store { atoms }) =
  (unsafeCoerce :: forall a. Foreign -> AtomSignal a)
    $ unsafePartial fromJust
    $ Object.lookup (reflectSymbol (SProxy :: _ key)) atoms

resetAtom :: forall key value atoms r. IsSymbol key => Cons key value r atoms => Atom key value -> Store atoms -> Effect Unit
resetAtom atom =
  getAtomSignal atom
    >>> case atom of
        Async a -> Async.reset a
        Sync a -> Sync.reset a
        Pure a -> Pure.reset a

updateAtom :: forall key value atoms r. IsSymbol key => Cons key value r atoms => value -> Atom key value -> Store atoms -> Effect Unit
updateAtom value atom =
  getAtomSignal atom
    >>> case atom of
        Async a -> Async.update value a
        Sync a -> Sync.update value a
        Pure a -> Pure.update value a
