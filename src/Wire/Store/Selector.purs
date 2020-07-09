module Wire.Store.Selector where

import Prelude
import Control.Monad.Free (Free, liftF, runFreeM)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Wire.Event (Event)
import Wire.Event as Event
import Wire.Signal (Signal)
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Atom

newtype Selector a
  = Selector
  { initial :: a
  , signal :: Signal a
  }

create ::
  forall a.
  { select :: Free Select a
  , update :: a -> Free Update Unit
  } ->
  Effect (Selector a)
create { select: select', update } = do
  let
    event = runEvent select'

    read' = runRead select'

    modify' f = read' >>= runUpdate <<< update <<< f
  initial <- read'
  pure $ Selector { initial, signal: { event, read: read', modify: modify' } }

unsafeCreate ::
  forall a.
  { select :: Free Select a
  , update :: a -> Free Update Unit
  } ->
  Selector a
unsafeCreate = unsafePerformEffect <<< create

instance atomSelector :: Atom Selector where
  default (Selector atom) = atom.initial
  read (Selector atom) = atom.signal.read
  modify f (Selector atom) = atom.signal.modify f
  reset _ = mempty
  subscribe notify (Selector atom) = Event.subscribe atom.signal.event notify
  signal (Selector atom) = atom.signal

newtype Select next
  = Select { event :: Event next, read :: Effect next }

derive instance functorSelect :: Functor Select

runEvent :: forall a. Free Select a -> Event a
runEvent = runFreeM case _ of Select s -> s.event

runRead :: forall a. Free Select a -> Effect a
runRead = runFreeM case _ of Select s -> s.read

select :: forall value atom. Atom atom => atom value -> Free Select value
select = \atom -> liftF $ Select { event: (Atom.signal atom).event, read: Atom.read atom }

data Update next
  = Update (Effect next)

derive instance functorUpdate :: Functor Update

runUpdate :: forall a. Free Update a -> Effect a
runUpdate = runFreeM case _ of Update next -> next

read :: forall atom value. Atom atom => atom value -> Free Update value
read atom = liftF $ Update $ Atom.read atom

modify :: forall atom value. Atom atom => (value -> value) -> atom value -> Free Update Unit
modify f atom = liftF $ Update $ Atom.modify f atom

write :: forall atom value. Atom atom => value -> atom value -> Free Update Unit
write = modify <<< const
