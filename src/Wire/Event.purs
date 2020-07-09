module Wire.Event where

import Prelude
import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
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

newtype Event a
  = Event ((a -> Effect Unit) -> Effect (Effect Unit))

create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit }
create = do
  subscribers <- Ref.new []
  let
    event =
      Event \notify -> do
        unsubscribing <- Ref.new false
        let
          isUnsubscribing = Ref.read unsubscribing

          subscriber = \a -> unlessM isUnsubscribing $ notify a
        Ref.modify_ (flip Array.snoc subscriber) subscribers
        pure do
          Ref.write true unsubscribing
          Ref.modify_ (Array.deleteBy unsafeRefEq subscriber) subscribers

    push a = Ref.read subscribers >>= traverse_ \notify -> notify a
  pure { event, push }

makeEvent :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Event a
makeEvent = Event

subscribe :: forall a b. Event a -> (a -> Effect b) -> Effect (Effect Unit)
subscribe (Event event) = event <<< (void <<< _)

filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter pred (Event event) = Event \notify -> event \a -> if pred a then notify a else pure unit

fold :: forall a b. (b -> a -> b) -> b -> Event a -> Event b
fold f b (Event event) =
  Event \notify -> do
    accum <- Ref.new b
    event \a -> do
      value <- Ref.modify (flip f a) accum
      notify value

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
      Event \notify -> do
        incrementCount
        cancel <- subscribe shared.event notify
        pure $ cancel *> decrementCount
  pure event

distinct :: forall a. Eq a => Event a -> Event a
distinct (Event event) =
  Event \notify -> do
    latest <- Ref.new Nothing
    event \a -> do
      b <- Ref.read latest
      when (pure a /= b) do
        Ref.write (pure a) latest
        notify a

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
  Event \notify -> do
    fiber <-
      Aff.launchAff
        $ for_ xs \x -> do
            liftEffect $ notify x
            Aff.delay (Milliseconds 0.0)
    pure
      $ Aff.launchAff_
      $ Aff.killFiber (Aff.error "canceled") fiber

range :: Int -> Int -> Event Int
range start end =
  Event \notify -> do
    let
      go pos
        | pos /= end = do
          liftEffect $ notify pos
          Aff.delay (Milliseconds 0.0)
          pure (Loop (pos + step))

      go _ = do
        liftEffect $ notify end
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
  map f (Event event) = Event \notify -> event (notify <<< f)

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
  pure a = Event \notify -> notify a *> mempty

instance bindEvent :: Bind Event where
  bind (Event outer) f =
    Event \notify -> do
      cancelInner <- Ref.new Nothing
      cancelOuter <-
        outer \a -> do
          Ref.read cancelInner >>= sequence_
          c <- subscribe (f a) notify
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
    Event \notify -> do
      cancel1 <- event1 notify
      cancel2 <- event2 notify
      pure $ cancel1 *> cancel2

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
