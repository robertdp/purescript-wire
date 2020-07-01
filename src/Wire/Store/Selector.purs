module Wire.Store.Selector where

import Prelude
import Control.Monad.Free.Trans (FreeT, freeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (maybe')
import Effect (Effect)
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.Store (Store)
import Wire.Store as Store
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Class

newtype Selector a
  = Selector
  { select :: FreeT StoreF Signal a
  , update :: a -> FreeT StoreF Effect Unit
  }

makeSelector ::
  forall a.
  { select :: FreeT StoreF Signal a
  , update :: a -> FreeT StoreF Effect Unit
  } ->
  Selector a
makeSelector = Selector

data StoreF next
  = Apply (Store -> next)

derive instance functorStoreF :: Functor StoreF

select :: forall atom value. Atom atom => atom value -> FreeT StoreF Signal value
select atom = freeT \_ -> pure $ Right $ Apply \store -> lift $ maybe' (\_ -> pure $ Class.defaultValue atom) _.signal $ Store.unsafeLookup atom store

read :: forall atom value. Atom atom => atom value -> FreeT StoreF Effect value
read atom = freeT \_ -> pure $ Right $ Apply \store -> lift $ maybe' (\_ -> pure $ Class.defaultValue atom) (Signal.read <<< _.signal) $ Store.unsafeLookup atom store

write :: forall atom value. Atom atom => atom value -> value -> FreeT StoreF Effect Unit
write atom value = freeT \_ -> pure $ Right $ Apply \store -> lift $ Store.update atom value store

interpret :: forall a m. MonadRec m => Store -> FreeT StoreF m a -> m a
interpret store = runFreeT \(Apply run) -> pure (run store)

build ::
  forall a.
  Selector a ->
  Store ->
  { signal :: Signal a
  , write :: a -> Effect Unit
  }
build (Selector selector) store =
  { signal: interpret store selector.select
  , write: \a -> interpret store (selector.update a)
  }
