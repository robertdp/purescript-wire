module Wire.Event.Queue where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff)
import Effect.Aff as AFf
import Effect.Aff as Aff
import Effect.Aff.AVar as AffVar
import Effect.Class (liftEffect)

create :: forall a b. (a -> Effect b) -> Effect { push :: Aff a -> Effect Unit, kill :: Effect Unit }
create consumer = do
  queue <- AVar.empty
  fiber <-
    (Aff.launchAff <<< forever) do
      aff <- AffVar.take queue
      a <- aff
      liftEffect do consumer a
  let
    killFiber = Aff.launchAff_ do AFf.killFiber (Aff.error "killing queue consumer") fiber

    killQueue = AVar.kill (Aff.error "killing queue") queue
  pure
    { push: \a -> Aff.launchAff_ do AffVar.put a queue
    , kill: killFiber *> killQueue
    }
