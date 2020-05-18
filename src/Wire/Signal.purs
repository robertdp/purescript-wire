module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

type Signal a
  = { event :: Event a
    , modify :: (a -> a) -> Effect Unit
    , read :: Effect a
    , write :: a -> Effect Unit
    }

create :: forall a. a -> Effect (Signal a)
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    read = Ref.read value

    write a = modify (const a)

    modify f = Ref.modify f value >>= inner.push >>> launchAff_

    event =
      Event.makeEvent \emit -> do
        liftEffect (Ref.read value) >>= emit
        Event.subscribe inner.event emit
  pure { event, read, write, modify }
