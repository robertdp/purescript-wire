module Wire.Store.Atom where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Wire.Store.Atom.Async (Async(..))
import Wire.Store.Atom.Pure (Pure(..))
import Wire.Store.Atom.Sync (Sync(..))
import Wire.Store.Atom.Types (Handler, Action)

data Atom (key :: Symbol) a
  = Async (Async a)
  | Sync (Sync a)
  | Pure (Pure a)

makeAsync :: forall a key. (Action a -> Handler Aff a) -> Atom key a
makeAsync = Async <<< Async'

makeSync :: forall a key. (Action a -> Handler Effect a) -> Atom key a
makeSync = Sync <<< Sync'

makePure :: forall a key. a -> Atom key a
makePure = Pure <<< Pure'
