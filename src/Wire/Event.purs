module Wire.Event where

import Prelude
import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Either (either, hush)
import Data.Filterable (class Compactable, class Filterable, filterMap, partitionMap)
import Data.Foldable (class Foldable, sequence_, traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AffVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

newtype Event a
  = Event (Subscribe a)

type Subscribe a
  = (a -> Aff Unit) -> Aff (Aff Unit)

create :: forall m n a. MonadEffect m => MonadAff n => m { event :: Event a, push :: a -> n Unit, cancel :: n Unit }
create =
  liftEffect do
    subscribers <- Ref.new []
    queue <- AVar.empty
    fiber <-
      Aff.launchAff do
        forever do
          a <- AffVar.take queue
          (liftEffect do Ref.read subscribers) >>= traverse_ \k -> k a
    let
      event =
        Event \emit ->
          liftEffect do
            unsubscribing <- Ref.new false
            let
              subscriber = \a -> unlessM (liftEffect do Ref.read unsubscribing) do emit a
            Ref.modify_ (flip Array.snoc subscriber) subscribers
            pure do
              liftEffect do
                Ref.write true unsubscribing
                Ref.modify_ (Array.deleteBy unsafeRefEq subscriber) subscribers

      push a = liftAff do AffVar.put a queue

      cancel =
        liftAff do
          Aff.sequential ado
            Aff.parallel do AffVar.kill (Aff.error "cancelled") queue
            Aff.parallel do Aff.killFiber (Aff.error "cancelled") fiber
            in unit
    pure { event, push, cancel }

makeEvent :: forall a. Subscribe a -> Event a
makeEvent = Event

subscribe :: forall a. Event a -> Subscribe a
subscribe (Event event) = event

filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter f (Event event) = Event \emit -> event \a -> when (f a) (emit a)

fold :: forall a b. (b -> a -> b) -> b -> Event a -> Event b
fold f b (Event event) =
  Event \emit -> do
    accum <- liftEffect do Ref.new b
    event \a -> (liftEffect do Ref.modify (flip f a) accum) >>= emit

share :: forall a. Event a -> Effect (Event a)
share source = do
  subscriberCount <- Ref.new 0
  cancelSource <- Ref.new Nothing
  shared <- create
  let
    incrementCount = do
      count <- liftEffect do Ref.modify (_ + 1) subscriberCount
      when (count == 1) do
        cancel <- subscribe source shared.push
        liftEffect do Ref.write (Just cancel) cancelSource

    decrementCount = do
      count <- liftEffect do Ref.modify (_ - 1) subscriberCount
      when (count == 0) do
        (liftEffect do Ref.read cancelSource) >>= sequence_
        liftEffect do Ref.write Nothing cancelSource

    event =
      Event \emit -> do
        incrementCount
        cancel <- subscribe shared.event emit
        pure do cancel *> decrementCount
  pure event

distinct :: forall a. Eq a => Event a -> Event a
distinct (Event event) =
  Event \emit -> do
    latest <- liftEffect do Ref.new Nothing
    event \a -> do
      b <- liftEffect do Ref.read latest
      when (pure a /= b) do
        liftEffect do Ref.write (pure a) latest
        emit a

bufferUntil :: forall b a. Event b -> Event a -> Event (Array a)
bufferUntil flush source =
  alt (Nothing <$ flush) (Just <$> source)
    # fold
        ( \{ buffer } -> case _ of
            Nothing -> { buffer: [], output: Just buffer }
            Just a -> { buffer: Array.snoc buffer a, output: Nothing }
        )
        { buffer: [], output: Nothing }
    # filterMap _.output

fromFoldable :: forall a f. Foldable f => f a -> Event a
fromFoldable xs =
  Event \emit -> do
    inner <- create
    cancel <- subscribe inner.event emit
    fiber <-
      Aff.forkAff do
        traverse_ inner.push xs
        cancel *> inner.cancel
    pure do
      Aff.sequential ado
        Aff.parallel do Aff.killFiber (Aff.error "cancelled") fiber
        Aff.parallel do cancel
        Aff.parallel do inner.cancel
        in unit

instance functorEvent :: Functor Event where
  map f (Event event) = Event \emit -> event \a -> emit (f a)

instance applyEvent :: Apply Event where
  apply (Event eventF) (Event eventA) =
    Event \emitB -> do
      latestF <- liftEffect do Ref.new Nothing
      latestA <- liftEffect do Ref.new Nothing
      { cancelF, cancelA } <-
        Aff.sequential ado
          cancelF <-
            Aff.parallel do
              eventF \f -> do
                liftEffect do Ref.write (Just f) latestF
                liftEffect (Ref.read latestA) >>= traverse_ \a -> emitB (f a)
          cancelA <-
            Aff.parallel do
              eventA \a -> do
                liftEffect do Ref.write (Just a) latestA
                liftEffect (Ref.read latestF) >>= traverse_ \f -> emitB (f a)
          in { cancelF, cancelA }
      pure do cancelF *> cancelA

instance applicativeEvent :: Applicative Event where
  pure a = fromFoldable [ a ]

instance bindEvent :: Bind Event where
  bind (Event outer) f =
    Event \emit -> do
      cancelInner <- liftEffect do Ref.new Nothing
      cancelOuter <-
        outer \a -> do
          (liftEffect do Ref.read cancelInner) >>= sequence_
          cancel <- subscribe (f a) emit
          liftEffect do Ref.write (Just cancel) cancelInner
      pure do
        (liftEffect do Ref.read cancelInner) >>= sequence_
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
  filterMap f = unsafePartial (map fromJust) <<< filter isJust <<< map f
  filter = filter
