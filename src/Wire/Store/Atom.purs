module Wire.Store.Atom where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Wire.Signal as Signal
import Wire.Store.Atom.Async (Async(..))
import Wire.Store.Atom.Async as Async
import Wire.Store.Atom.Pure (Pure(..))
import Wire.Store.Atom.Sync (Sync(..))
import Wire.Store.Atom.Sync as Sync
import Wire.Store.Atom.Types (AtomStore, Lifecycle, Handler)

data Atom (key :: Symbol) a
  = Async (Async a)
  | Sync (Sync a)
  | Pure (Pure a)

makeAsync :: forall a key. (Lifecycle a -> Handler Aff a) -> Atom key a
makeAsync = Async <<< Async'

makeSync :: forall a key. (Lifecycle a -> Handler Effect a) -> Atom key a
makeSync = Sync <<< Sync'

makePure :: forall a key. a -> Atom key a
makePure = Pure <<< Pure'
