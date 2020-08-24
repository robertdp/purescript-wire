module Wire.Event where

import Prelude
import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Array as Array
import Data.Either (either, hush)
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

newtype Event a
  = Event ((a -> Effect Unit) -> Effect (Effect Unit))

create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit }
create = do
  subscribers <- Ref.new []
  let
    event =
      Event \emit -> do
        unsubscribing <- Ref.new false
        let
          isUnsubscribing = Ref.read unsubscribing

          subscriber = \a -> unlessM isUnsubscribing do emit a
        Ref.modify_ (_ <> [ subscriber ]) subscribers
        pure do
          Ref.write true unsubscribing
          Ref.modify_ (Array.deleteBy unsafeRefEq subscriber) subscribers

    push a = Ref.read subscribers >>= traverse_ \emit -> emit a
  pure { event, push }

makeEvent :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Event a
makeEvent = Event

subscribe :: forall a b. Event a -> (a -> Effect b) -> Effect (Effect Unit)
subscribe (Event event) = event <<< (void <<< _)

filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter pred (Event event) = Event \emit -> event \a -> if pred a then emit a else pure unit

fold :: forall a b. (b -> a -> b) -> b -> Event a -> Event b
fold f b (Event event) =
  Event \emit -> do
    accum <- Ref.new b
    event \a -> do
      value <- Ref.modify (flip f a) accum
      emit value

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
        pure $ cancel *> decrementCount
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

bufferUntil :: forall b a. Event a -> Event b -> Event (Array a)
bufferUntil source flush =
  alt (Just <$> source) (Nothing <$ flush)
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
      Aff.launchAff
        $ for_ xs \x -> do
            liftEffect $ emit x
            Aff.delay (Milliseconds 0.0)
    pure
      $ Aff.launchAff_
      $ Aff.killFiber (Aff.error "canceled") fiber

range :: Int -> Int -> Event Int
range start end =
  Event \emit -> do
    let
      go pos
        | pos /= end = do
          liftEffect $ emit pos
          Aff.delay (Milliseconds 0.0)
          pure (Loop (pos + step))

      go _ = do
        liftEffect $ emit end
        pure (Done unit)
    fiber <- Aff.launchAff $ tailRecM go start
    pure
      $ Aff.launchAff_
      $ Aff.killFiber (Aff.error "canceled") fiber
  where
  step = if start < end then 1 else -1

times :: Int -> Event Int
times n
  | n > 0 = range 1 n

times _ = empty

instance functorEvent :: Functor Event where
  map f (Event event) = Event \emit -> event (emit <<< f)

instance applyEvent :: Apply Event where
  apply = ap

instance applicativeEvent :: Applicative Event where
  pure a = Event \emit -> emit a *> mempty

instance bindEvent :: Bind Event where
  bind (Event outer) f =
    Event \emit -> do
      cancelInner <- Ref.new Nothing
      cancelOuter <-
        outer \a -> do
          Ref.read cancelInner >>= sequence_
          c <- subscribe (f a) emit
          Ref.write (Just c) cancelInner
      pure do
        Ref.read cancelInner >>= sequence_
        cancelOuter

instance monadEvent :: Monad Event

instance monadRecEvent ∷ MonadRec Event where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b

instance plusEvent :: Plus Event where
  empty = Event \_ -> mempty

instance alternativeEvent :: Alternative Event

instance altEvent :: Alt Event where
  alt (Event event1) (Event event2) =
    Event \emit -> do
      cancel1 <- event1 emit
      cancel2 <- event2 emit
      pure $ cancel1 *> cancel2

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
