module Wire.Store.Selector where

import Prelude
import Control.Monad.Free.Trans (FreeT, freeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.Store (Store)
import Wire.Store as Store
import Wire.Store.Atom.Class (class Atom)

newtype Selector (atoms :: # Type) a
  = Selector
  { select :: FreeT (SelectF atoms) Signal a
  , update :: a -> FreeT (SelectF atoms) Effect Unit
  }

makeSelector ::
  forall a atoms.
  { select :: FreeT (SelectF atoms) Signal a
  , update :: a -> FreeT (SelectF atoms) Effect Unit
  } ->
  Selector atoms a
makeSelector = Selector

data SelectF (atoms :: # Type) next
  = Apply (Store atoms -> next)

derive instance functorSelectF :: Functor (SelectF atoms)

select ::
  forall atom key r atoms value.
  Atom atom key value =>
  IsSymbol key =>
  Cons key value r atoms =>
  atom ->
  FreeT (SelectF atoms) Signal value
select atom = freeT \_ -> pure $ Right $ Apply \store -> lift (Store.getAtom atom store).signal

read ::
  forall atom value r atoms key.
  Atom atom key value =>
  IsSymbol key =>
  Cons key value r atoms =>
  atom ->
  FreeT (SelectF atoms) Effect value
read atom = freeT \_ -> pure $ Right $ Apply \store -> lift $ Signal.read (Store.getAtom atom store).signal

write ::
  forall atom r atoms value key.
  Atom atom key value =>
  IsSymbol key =>
  Cons key value r atoms =>
  value ->
  atom ->
  FreeT (SelectF atoms) Effect Unit
write value atom = freeT \_ -> pure $ Right $ Apply \store -> lift $ Store.updateAtom atom value store

interpret :: forall a m atoms. MonadRec m => Store atoms -> FreeT (SelectF atoms) m a -> m a
interpret store = runFreeT \(Apply run) -> pure (run store)

build ::
  forall a atoms.
  Selector atoms a ->
  Store atoms ->
  { signal :: Signal a
  , write :: a -> Effect Unit
  }
build (Selector selector) store =
  { signal: interpret store selector.select
  , write: \a -> interpret store (selector.update a)
  }
