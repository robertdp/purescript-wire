module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

type Signal a
  = { event :: Event a
    , read :: Effect a
    , write :: a -> Effect Unit
    , modify :: (a -> a) -> Effect Unit
    }

create :: forall a. a -> Effect (Signal a)
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    read = Ref.read value

    write a = modify (const a)

    modify f = Ref.modify f value >>= inner.push

    event =
      Event.makeEvent \emit -> do
        _ <- Ref.read value >>= emit
        Event.subscribe inner.event emit
  pure { event, read, write, modify }
