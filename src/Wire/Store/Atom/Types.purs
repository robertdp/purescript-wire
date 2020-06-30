module Wire.Store.Atom.Types where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.Signal (Signal)
import Wire.Signal as Signal

data AtomicF a next
  = Read (Maybe a -> next)
  | Write a next

derive instance functorAtomicF :: Functor (AtomicF a)

type Handler m a
  = FreeT (AtomicF a) m Unit

read :: forall a m. Monad m => FreeT (AtomicF a) m (Maybe a)
read = liftFreeT $ Read identity

write :: forall a m. Monad m => a -> FreeT (AtomicF a) m Unit
write a = liftFreeT $ Write a unit

interpret :: forall a m. MonadEffect m => MonadRec m => StoreSignal a -> FreeT (AtomicF a) m Unit -> m Unit
interpret store =
  runFreeT case _ of
    Read next -> do
      a <- liftEffect (Signal.read store.signal)
      pure (next a)
    Write a next -> do
      liftEffect (store.write (pure a))
      pure next

type StoreSignal a
  = { signal :: Signal (Maybe a)
    , write :: Maybe a -> Effect Unit
    , modify :: (Maybe a -> Maybe a) -> Effect Unit
    , cancel :: Effect Unit
    }

data Action a
  = Initialize
  | Update a

derive instance functorAction :: Functor Action

instance foldableAction :: Foldable Action where
  foldr _ z Initialize = z
  foldr f z (Update x) = x `f` z
  foldl _ z Initialize = z
  foldl f z (Update x) = z `f` x
  foldMap f Initialize = mempty
  foldMap f (Update x) = f x
