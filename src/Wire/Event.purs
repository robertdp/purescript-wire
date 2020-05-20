module Wire.Event where

import Prelude
import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Apply (lift2)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.Filterable (class Compactable, class Filterable, filterMap, partitionMap)
import Data.Foldable (class Foldable, for_, sequence_, traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)
import Wire.Event.Queue as Queue

newtype Event a
  = Event (Subscriber a -> Effect Canceler)

type Subscriber a
  = a -> Effect Unit

type Canceler
  = Effect Unit

create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit, cancel :: Effect Unit }
create = do
  subscribers <- Ref.new []
  queue <- Queue.create \a -> Ref.read subscribers >>= traverse_ \k -> k a
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
  pure { event, push: queue.push, cancel: queue.kill }

makeEvent :: forall a. (Subscriber a -> Effect Canceler) -> Event a
makeEvent = Event

subscribe :: forall a. Event a -> Subscriber a -> Effect Canceler
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
      count <- liftEffect do Ref.modify (_ + 1) subscriberCount
      when (count == 1) do
        cancel <- subscribe source do liftEffect <<< shared.push
        liftEffect do Ref.write (Just cancel) cancelSource

    decrementCount = do
      count <- liftEffect do Ref.modify (_ - 1) subscriberCount
      when (count == 0) do
        liftEffect (Ref.read cancelSource) >>= sequence_
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
    latest <- Ref.new Nothing
    event \a -> do
      b <- Ref.read latest
      when (pure a /= b) do
        Ref.write (pure a) latest
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
    fiber <-
      Aff.launchAff do
        for_ xs \x -> do
          liftEffect do emit x
          Aff.delay (Milliseconds 0.0)
    pure do
      Aff.launchAff_ do Aff.killFiber (Aff.error "canceled") fiber

range :: Int -> Int -> Event Int
range start end =
  Event \emit -> do
    let
      go pos
        | pos /= end = do
          liftEffect do emit pos
          Aff.delay (Milliseconds 0.0)
          pure (Loop (pos + step))

      go _ = do
        liftEffect do emit end
        pure (Done unit)
    fiber <- Aff.launchAff do tailRecM go start
    pure do
      Aff.launchAff_ do Aff.killFiber (Aff.error "canceled") fiber
  where
  step = if start < end then 1 else -1

times :: Int -> Event Int
times n
  | n > 0 = range 1 n

times _ = empty

instance functorEvent :: Functor Event where
  map f (Event event) =
    Event \emit -> do
      queue <- Queue.create (emit <<< f)
      cancel <- event queue.push
      pure do
        cancel
        queue.kill

instance applyEvent :: Apply Event where
  apply eventF eventA =
    alt (Left <$> eventF) (Right <$> eventA)
      # fold
          ( \{ left, right } -> case _ of
              Left l -> { left: Just l, right }
              Right r -> { left, right: Just r }
          )
          { left: Nothing, right: Nothing }
      # filterMap (\{ left, right } -> apply left right)

instance applicativeEvent :: Applicative Event where
  pure a = Event \emit -> emit a *> mempty

instance bindEvent :: Bind Event where
  bind (Event outer) f =
    Event \emit -> do
      cancelInner <- Ref.new Nothing
      cancelOuter <-
        outer \a ->
          liftEffect do
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
  filterMap f = unsafePartial (map fromJust) <<< filter isJust <<< map f
  filter = filter
