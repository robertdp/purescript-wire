module Wire.Signal
  ( Signal(..)
  , Signal'
  , create
  , distinct
  , filter
  , readOnly
  , restrict
  , module Exports
  ) where

import Prelude
import Data.Array (deleteBy, snoc)
import Data.Bitraversable (class Bitraversable, ltraverse)
import Data.Either (Either)
import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, rmap)
import Effect (Effect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Wire.Signal.Class (class Readable, class Writable, immediately, modify, read, subscribe, write) as Exports

newtype Signal i o
  = Signal
  { read :: Effect o
  , write :: i -> Effect Unit
  , subscribe :: (o -> Effect Unit) -> Effect (Effect Unit)
  }

type Signal' a
  = Signal a a

create :: forall a. a -> Effect (Signal' a)
create init = ado
  value <- Ref.new init
  subscribers <- Ref.new []
  let
    read = Ref.read value

    write a = do
      Ref.write a value
      Ref.read subscribers >>= traverse_ \k -> k a

    subscribe k = do
      unsubscribing <- Ref.new false
      let
        subscriber = \a -> unlessM (Ref.read unsubscribing) do k a
      Ref.modify_ (flip snoc subscriber) subscribers
      pure do
        Ref.write true unsubscribing
        Ref.modify_ (deleteBy unsafeRefEq subscriber) subscribers
  in Signal { read, write, subscribe }

readOnly :: forall i o. Signal i o -> Signal Void o
readOnly = unsafeCoerce

distinct :: forall i o. Eq o => Signal i o -> Signal i o
distinct (Signal s) = Signal s { subscribe = subscribe }
  where
  subscribe k = do
    lastRef <- Ref.new Nothing
    s.subscribe \a -> do
      last <- Ref.read lastRef
      when (pure a /= last) do
        Ref.write (pure a) lastRef
        k a

restrict :: forall i o. (i -> Boolean) -> Signal i o -> Signal i o
restrict predicate (Signal s) = Signal s { write = write }
  where
  write a = when (predicate a) do s.write a

filter :: forall i o. (o -> Boolean) -> Signal i o -> Signal i o
filter predicate (Signal s) = Signal s { subscribe = subscribe }
  where
  subscribe k = s.subscribe \a -> when (predicate a) do k a

traversable :: forall i o f. Foldable f => Signal i o -> Signal (f i) o
traversable (Signal s) = Signal s { write = traverse_ s.write }

ltraversable :: forall i o a f. Bitraversable f => Signal i o -> Signal (f i a) o
ltraversable (Signal s) = Signal s { write = void <<< ltraverse s.write }

restrictJust :: forall i o. Signal i o -> Signal (Maybe i) o
restrictJust = traversable

restrictRight :: forall i o a. Signal i o -> Signal (Either a i) o
restrictRight = traversable

restrictLeft :: forall i o a. Signal i o -> Signal (Either i a) o
restrictLeft = ltraversable

instance readableSignal :: Exports.Readable Signal Effect where
  read (Signal s) = s.read
  subscribe k (Signal s) = s.subscribe k

instance writableSignal :: Exports.Writable Signal Effect where
  write a (Signal s) = s.write a

instance profunctorSignal :: Profunctor Signal where
  dimap f g (Signal s) = Signal { read, write, subscribe }
    where
    read = map g s.read

    write a = s.write (f a)

    subscribe k = s.subscribe \a -> k (g a)

instance functorSignal :: Functor (Signal i) where
  map = rmap
