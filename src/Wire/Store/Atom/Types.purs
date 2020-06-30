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
  = Modify (a -> a) (a -> next)

derive instance functorAtomicF :: Functor (AtomicF a)

type Handler m a
  = FreeT (AtomicF a) m Unit

read :: forall a m. Monad m => FreeT (AtomicF a) m a
read = liftFreeT $ Modify identity identity

write :: forall a m. Monad m => a -> FreeT (AtomicF a) m Unit
write a = liftFreeT $ Modify (const a) (const unit)

modify :: forall m a. Monad m => (a -> a) -> FreeT (AtomicF a) m a
modify f = liftFreeT $ Modify f identity

modify_ :: forall m a. Monad m => (a -> a) -> FreeT (AtomicF a) m Unit
modify_ f = liftFreeT $ Modify f (const unit)

interpret :: forall a m. MonadEffect m => MonadRec m => StoreSignal a -> FreeT (AtomicF a) m Unit -> m Unit
interpret store =
  runFreeT case _ of
    Modify f next -> do
      liftEffect (store.modify f)
      a <- liftEffect (Signal.read store.signal)
      pure (next a)

type StoreSignal a
  = { signal :: Signal a
    , write :: a -> Effect Unit
    , modify :: (a -> a) -> Effect Unit
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
