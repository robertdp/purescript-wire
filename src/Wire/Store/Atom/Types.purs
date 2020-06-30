module Wire.Store.Atom.Types where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.Signal (Signal)
import Wire.Signal as Signal

data StateF a next
  = State (a -> a) (a -> next)

derive instance functorStateF :: Functor (StateF a)

type Handler m a
  = FreeT (StateF a) m Unit

read :: forall a m. Monad m => FreeT (StateF a) m a
read = liftFreeT $ State identity identity

write :: forall a m. Monad m => a -> FreeT (StateF a) m Unit
write a = liftFreeT $ State (const a) (const unit)

modify :: forall m a. Monad m => (a -> a) -> FreeT (StateF a) m a
modify f = liftFreeT $ State f identity

modify_ :: forall m a. Monad m => (a -> a) -> FreeT (StateF a) m Unit
modify_ f = liftFreeT $ State f (const unit)

interpret :: forall a m. MonadEffect m => MonadRec m => StoreSignal a -> FreeT (StateF a) m Unit -> m Unit
interpret store =
  runFreeT case _ of
    State f next -> do
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
