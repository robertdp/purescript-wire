module Wire.Signal where

import Prelude
import Data.Array (deleteBy, snoc)
import Data.Foldable (traverse_)
import Data.Profunctor (class Profunctor)
import Effect (Effect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Wire.Class (class Readable, class Writable)

newtype Signal i o
  = Signal
  { read :: Effect o
  , subscribe :: (o -> Effect Unit) -> Effect (Effect Unit)
  , write :: i -> Effect Unit
  }

readOnly :: forall i o. Signal i o -> Signal Void o
readOnly = unsafeCoerce

instance readableSignal :: Readable (Signal i) Effect where
  read (Signal s) = s.read
  subscribe (Signal s) = s.subscribe

instance writableSignal :: Writable (Signal) Effect where
  write (Signal s) = s.write

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

instance profunctorSignal :: Profunctor Signal where
  dimap f g (Signal s) =
    Signal
      { read: g <$> s.read
      , subscribe: \k -> s.subscribe (k <<< g)
      , write: s.write <<< f
      }

instance functorSignal :: Functor (Signal i) where
  map f (Signal s) =
    Signal
      { read: f <$> s.read
      , subscribe: \k -> s.subscribe (k <<< f)
      , write: s.write
      }
