module Wire.Store.Selector where

import Prelude
import Control.Monad.Free.Trans (FreeT, freeT)
import Control.Monad.Maybe.Trans (MaybeT(..), lift)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import Wire.Signal (Signal)
import Wire.Store (Store)
import Wire.Store as Store
import Wire.Store.Atom (Atom)

data Selector key (atoms :: # Type) a
  = Selector' (FreeT (SelectorF atoms) (MaybeT Signal) a) (a -> Effect Unit)

data SelectorF (atoms :: # Type) next
  = Select (Store atoms -> next)

derive instance functorSelectorF :: Functor (SelectorF atoms)

select :: forall key r atoms value. IsSymbol key => Cons key value r atoms => Atom key value -> FreeT (SelectorF atoms) (MaybeT Signal) value
select atom =
  freeT \_ ->
    pure $ Right $ Select \store -> lift $ MaybeT (Store.lookup atom store).signal
