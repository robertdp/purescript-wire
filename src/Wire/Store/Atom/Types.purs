module Wire.Store.Atom.Types where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.Signal (Signal)
import Wire.Signal as Signal

data AtomicF a next
  = Read (a -> next)
  | Write a next

derive instance functorAtomicF :: Functor (AtomicF a)

type Handler m a
  = FreeT (AtomicF a) m Unit

read :: forall a m. Monad m => FreeT (AtomicF a) m a
read = liftFreeT $ Read identity

write :: forall a m. Monad m => a -> FreeT (AtomicF a) m Unit
write a = liftFreeT $ Write a unit

interpret :: forall a m. MonadEffect m => MonadRec m => AtomSignal a -> FreeT (AtomicF a) m Unit -> m Unit
interpret store =
  runFreeT case _ of
    Read next -> do
      a <- liftEffect (Signal.read store.signal)
      pure (next a)
    Write a next -> do
      liftEffect (store.write a)
      pure next

type AtomSignal a
  = { signal :: Signal a
    , write :: a -> Effect Unit
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
