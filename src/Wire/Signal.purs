module Wire.Signal
  ( Signal(..)
  , create
  , readOnly
  , module Exports
  ) where

import Prelude
import Data.Array (deleteBy, snoc)
import Data.Foldable (traverse_)
import Data.Profunctor (class Profunctor, rmap)
import Effect (Effect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Wire.Signal.Class (class Readable, class Writable)
import Wire.Signal.Class (class Readable, class Writable, read, subscribe, subscribe', write) as Exports

newtype Signal i o
  = Signal
  { read :: Effect o
  , subscribe :: (o -> Effect Unit) -> Effect (Effect Unit)
  , write :: i -> Effect Unit
  }

create :: forall a. a -> Effect (Signal a a)
create init = ado
  value <- Ref.new init
  subscribers <- Ref.new []
  let
    read = Ref.read value

    subscribe k = do
      unsubscribing <- Ref.new false
      let
        subscriber = \a -> unlessM (Ref.read unsubscribing) (k a)
      Ref.modify_ (flip snoc subscriber) subscribers
      pure do
        Ref.write true unsubscribing
        Ref.modify_ (deleteBy unsafeRefEq subscriber) subscribers

    write a = do
      Ref.write a value
      Ref.read subscribers >>= traverse_ \k -> k a
  in Signal { read, write, subscribe }

readOnly :: forall i o. Signal i o -> Signal Void o
readOnly = unsafeCoerce

instance readableSignal :: Readable (Signal i) Effect where
  read (Signal s) = s.read
  subscribe (Signal s) = s.subscribe

instance writableSignal :: Writable (Signal) Effect where
  write (Signal s) = s.write

instance profunctorSignal :: Profunctor Signal where
  dimap f g (Signal s) =
    Signal
      { read: map g s.read
      , subscribe: \k -> s.subscribe (k <<< g)
      , write: s.write <<< f
      }

instance functorSignal :: Functor (Signal i) where
  map = rmap
