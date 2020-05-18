module Wire.Signal where

import Prelude
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

type Signal m a
  = { event :: Event a
    , modify :: (a -> a) -> m Unit
    , read :: m a
    , write :: a -> m Unit
    }

create ::
  forall effect aff a.
  MonadEffect effect =>
  MonadAff aff =>
  a ->
  effect
    { event :: Event a
    , modify :: (a -> a) -> aff Unit
    , read :: effect a
    , write :: a -> aff Unit
    }
create init =
  liftEffect do
    value <- Ref.new init
    inner <- Event.create
    let
      read = liftEffect do Ref.read value

      write a = modify (const a)

      modify f = liftAff do liftEffect (Ref.modify f value) >>= inner.push

      event =
        Event.makeEvent \emit -> do
          liftEffect (Ref.read value) >>= emit
          Event.subscribe inner.event emit
    pure { event, read, write, modify }
