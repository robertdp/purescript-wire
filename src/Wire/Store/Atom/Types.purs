module Wire.Store.Atom.Types where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.Signal (Signal)
import Wire.Signal as Signal

data AtomF a next
  = Peek (Maybe a -> next)
  | Poke a next

derive instance functorAtomF :: Functor (AtomF a)

type Handler m a
  = FreeT (AtomF a) m Unit

peek :: forall a m. Monad m => FreeT (AtomF a) m (Maybe a)
peek = liftFreeT $ Peek identity

poke :: forall a m. Monad m => a -> FreeT (AtomF a) m Unit
poke a = liftFreeT $ Poke a unit

interpret :: forall a m. MonadEffect m => MonadRec m => AtomSignal a -> FreeT (AtomF a) m Unit -> m Unit
interpret store =
  runFreeT case _ of
    Peek next -> do
      a <- liftEffect (Signal.read store.signal)
      pure (next a)
    Poke a next -> do
      liftEffect (store.write (pure a))
      pure next

type AtomSignal a
  = { signal :: Signal (Maybe a)
    , write :: Maybe a -> Effect Unit
    }

createEmptySignal :: forall a. Effect (AtomSignal a)
createEmptySignal = do
  { signal, write } <- Signal.create Nothing
  pure { signal, write }

data Lifecycle a
  = Init
  | Update a

derive instance functorLifecycle :: Functor Lifecycle

instance foldableMaybe :: Foldable Lifecycle where
  foldr _ z Init = z
  foldr f z (Update x) = x `f` z
  foldl _ z Init = z
  foldl f z (Update x) = z `f` x
  foldMap f Init = mempty
  foldMap f (Update x) = f x
