module Wire.Event where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Data.Array as Array
import Data.Either (either, hush)
import Data.Filterable (class Compactable, class Filterable, filterMap, partitionMap)
import Data.Foldable (class Foldable, sequence_, traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

newtype Event a
  = Event (Subscribe a)

type Subscribe a
  = (a -> Effect Unit) -> Effect Canceler

type Canceler
  = Effect Unit

create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit }
create = do
  subscribers <- Ref.new []
  let
    event =
      Event \emit -> do
        unsubscribing <- Ref.new false
        let
          subscriber = \a -> unlessM (Ref.read unsubscribing) do emit a
        Ref.modify_ (flip Array.snoc subscriber) subscribers
        pure do
          Ref.write true unsubscribing
          Ref.modify_ (Array.deleteBy unsafeRefEq subscriber) subscribers

    push a = Ref.read subscribers >>= traverse_ \emit -> emit a
  pure { event, push }

makeEvent :: forall a. Subscribe a -> Event a
makeEvent = Event

subscribe :: forall a. Event a -> Subscribe a
subscribe (Event event) = event

filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter f (Event event) = Event \emit -> event \a -> when (f a) (emit a)

fold :: forall a b. (b -> a -> b) -> b -> Event a -> Event b
fold f b (Event event) =
  Event \emit -> do
    accum <- Ref.new b
    event \a -> Ref.modify (flip f a) accum >>= emit

share :: forall a. Event a -> Effect (Event a)
share source = do
  subscriberCount <- Ref.new 0
  cancelSource <- Ref.new Nothing
  shared <- create
  let
    incrementCount = do
      count <- Ref.modify (_ + 1) subscriberCount
      when (count == 1) do
        cancel <- subscribe source shared.push
        Ref.write (Just cancel) cancelSource

    decrementCount = do
      count <- Ref.modify (_ - 1) subscriberCount
      when (count == 0) do
        Ref.read cancelSource >>= sequence_
        Ref.write Nothing cancelSource

    event =
      Event \emit -> do
        incrementCount
        cancel <- subscribe shared.event emit
        pure do cancel *> decrementCount
  pure event

distinct :: forall a. Eq a => Event a -> Event a
distinct (Event event) =
  Event \emit -> do
    latest <- Ref.new Nothing
    event \a -> do
      b <- Ref.read latest
      when (pure a /= b) do
        Ref.write (pure a) latest
        emit a

buffer :: forall a b. Event b -> Event a -> Event (Array a)
buffer (Event flush) (Event event) =
  Event \emit -> do
    values <- Ref.new []
    cancelFlush <- flush \_ -> Ref.modify' { state: [], value: _ } values >>= emit
    cancelEvent <- event \a -> Ref.modify_ (flip Array.snoc a) values
    pure do cancelEvent *> cancelFlush

fromFoldable :: forall a f. Foldable f => f a -> Event a
fromFoldable xs = Event \emit -> traverse_ emit xs *> mempty

instance functorEvent :: Functor Event where
  map f (Event event) = Event \emit -> event \a -> emit (f a)

instance applyEvent :: Apply Event where
  apply (Event eventF) (Event eventA) =
    Event \emitB -> do
      latestF <- Ref.new Nothing
      latestA <- Ref.new Nothing
      cancelF <-
        eventF \f -> do
          Ref.write (Just f) latestF
          Ref.read latestA >>= traverse_ \a -> emitB (f a)
      cancelA <-
        eventA \a -> do
          Ref.write (Just a) latestA
          Ref.read latestF >>= traverse_ \f -> emitB (f a)
      pure do cancelF *> cancelA

instance applicativeEvent :: Applicative Event where
  pure a = Event \emit -> emit a *> mempty

instance bindEvent :: Bind Event where
  bind (Event outer) f =
    Event \emit -> do
      cancelInner <- Ref.new Nothing
      cancelOuter <-
        outer \a -> do
          Ref.read cancelInner >>= sequence_
          cancel <- subscribe (f a) emit
          Ref.write (Just cancel) cancelInner
      pure do
        Ref.read cancelInner >>= sequence_
        cancelOuter

instance monadEvent :: Monad Event

instance plusEvent :: Plus Event where
  empty = Event \_ -> mempty

instance alternativeEvent :: Alternative Event

instance altEvent :: Alt Event where
  alt (Event event1) (Event event2) =
    Event \emit -> do
      cancel1 <- event1 emit
      cancel2 <- event2 emit
      pure do cancel1 *> cancel2

instance semigroupEvent :: Semigroup a => Semigroup (Event a) where
  append = lift2 append

instance monoidEvent :: Monoid a => Monoid (Event a) where
  mempty = pure mempty

instance compactableEvent :: Compactable Event where
  compact = filterMap identity
  separate = partitionMap identity

instance filterableEvent :: Filterable Event where
  partitionMap f event =
    { left: filterMap (either Just (const Nothing) <<< f) event
    , right: filterMap (hush <<< f) event
    }
  partition f event =
    { yes: filter f event
    , no: filter (not f) event
    }
  filterMap f = map (unsafePartial fromJust) <<< filter isJust <<< map f
  filter = filter
