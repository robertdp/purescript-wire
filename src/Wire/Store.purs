module Wire.Store where

import Prelude
import Data.Maybe (Maybe, fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons, class Lacks)
import Unsafe.Coerce (unsafeCoerce)
import Wire.Signal (Signal)
import Wire.Store.Atom (Atom(..))
import Wire.Store.Atom.Async as Async
import Wire.Store.Atom.Pure as Pure
import Wire.Store.Atom.Sync as Sync

newtype Store (atoms :: # Type)
  = Store { atoms :: Object Compressed }

empty :: Store ()
empty = Store { atoms: Object.empty }

insert :: forall key value before after. IsSymbol key => Lacks key before => Cons key value before after => Atom key value -> Store before -> Effect (Store after)
insert atom (Store store) = do
  atomStore <- case atom of
    Async a -> Async.createStore a
    Sync a -> Sync.createStore a
    Pure a -> Pure.createStore a
  pure $ Store store { atoms = Object.insert (reflectSymbol (SProxy :: _ key)) (compress atomStore) store.atoms }

lookup :: forall key value atoms r. IsSymbol key => Cons key value r atoms => Atom key value -> Store atoms -> AtomStore value
lookup _ (Store { atoms }) = inflate $ unsafePartial fromJust $ Object.lookup (reflectSymbol (SProxy :: _ key)) atoms

reset :: forall key value atoms r. IsSymbol key => Cons key value r atoms => Atom key value -> Store atoms -> Effect Unit
reset atom =
  lookup atom
    >>> case atom of
        Async a -> Async.reset a
        Sync a -> Sync.reset a
        Pure a -> Pure.reset a

update :: forall key value atoms r. IsSymbol key => Cons key value r atoms => value -> Atom key value -> Store atoms -> Effect Unit
update value atom =
  lookup atom
    >>> case atom of
        Async a -> Async.update value a
        Sync a -> Sync.update value a
        Pure a -> Pure.update value a

type AtomStore a
  = { signal :: Signal (Maybe a), write :: Maybe a -> Effect Unit }

data Compressed

compress :: forall a. AtomStore a -> Compressed
compress = unsafeCoerce

inflate :: forall a. Compressed -> AtomStore a
inflate = unsafeCoerce
