module Wire.React.Selector where

import Prelude
import Control.Monad.Free (Free, liftF, runFreeM)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Wire.React.Class (class Atom)
import Wire.React.Class as Class
import Wire.Signal (Signal)
import Wire.Signal as Signal

newtype Selector a
  = Selector
  { initial :: a
  , signal :: Signal a
  , modify :: (a -> a) -> Effect Unit
  }

create ::
  forall a.
  { select :: Free Select a
  , update :: a -> Free Update Unit
  } ->
  Effect (Selector a)
create { select: select', update } = do
  let
    signal = runSelect select'

    modify' f = Signal.read signal >>= runUpdate <<< update <<< f
  initial <- Signal.read signal
  pure $ Selector { initial, signal, modify: modify' }

unsafeCreate ::
  forall a.
  { select :: Free Select a
  , update :: a -> Free Update Unit
  } ->
  Selector a
unsafeCreate = unsafePerformEffect <<< create

instance atomSelector :: Atom Selector where
  default (Selector atom) = atom.initial
  read (Selector atom) = Signal.read atom.signal
  modify (Selector atom) = atom.modify
  reset _ = mempty
  subscribe (Selector atom) = Signal.subscribe atom.signal
  signal (Selector atom) = atom.signal

newtype Select next
  = Select (Signal next)

derive instance functorSelect :: Functor Select

runSelect :: Free Select ~> Signal
runSelect = runFreeM case _ of Select s -> s

select :: forall atom. Atom atom => atom ~> Free Select
select = \atom -> liftF $ Select $ Class.signal atom

data Update next
  = Update (Effect next)

derive instance functorUpdate :: Functor Update

runUpdate :: Free Update ~> Effect
runUpdate = runFreeM case _ of Update next -> next

read :: forall atom. Atom atom => atom ~> Free Update
read atom = liftF $ Update $ Class.read atom

modify :: forall atom value. Atom atom => atom value -> (value -> value) -> Free Update Unit
modify atom f = liftF $ Update $ Class.modify atom f

write :: forall atom value. Atom atom => atom value -> value -> Free Update Unit
write atom a = modify atom (const a)
