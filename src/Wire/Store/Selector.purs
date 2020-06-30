module Wire.Store.Selector where

import Prelude
import Control.Monad.Free.Trans (FreeT, freeT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.Store (Store)
import Wire.Store as Store
import Wire.Store.Atom (Atom)

newtype Selector key (atoms :: # Type) a
  = Selector'
  { select :: FreeT (SelectF atoms) Signal a
  , update :: a -> FreeT (SelectF atoms) Effect Unit
  }

makeSelector ::
  forall a atoms key.
  { select :: FreeT (SelectF atoms) Signal a
  , update :: a -> FreeT (SelectF atoms) Effect Unit
  } ->
  Selector key atoms a
makeSelector = Selector'

data SelectF (atoms :: # Type) next
  = Apply (Store atoms -> next)

derive instance functorSelectF :: Functor (SelectF atoms)

select :: forall key r atoms value. IsSymbol key => Cons key value r atoms => Atom key value -> FreeT (SelectF atoms) Signal value
select atom = freeT \_ -> pure $ Right $ Apply \store -> lift (Store.lookup atom store).signal

read :: forall value r atoms key. IsSymbol key => Cons key value r atoms => Atom key value -> FreeT (SelectF atoms) Effect value
read atom = freeT \_ -> pure $ Right $ Apply \store -> lift $ Signal.read (Store.lookup atom store).signal

write :: forall r atoms value key. IsSymbol key => Cons key value r atoms => value -> Atom key value -> FreeT (SelectF atoms) Effect Unit
write value atom = freeT \_ -> pure $ Right $ Apply \store -> lift $ (Store.lookup atom store).write value
