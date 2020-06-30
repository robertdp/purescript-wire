module Wire.Store.Builder where

import Prelude
import Data.Profunctor.Star (Star(..))
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import Wire.Store (Store)
import Wire.Store as Store
import Wire.Store.Atom.Class (class Atom)

newtype Builder i o
  = Builder (Star Effect (Store i) (Store o))

instance semigroupoidBuilder :: Semigroupoid Builder where
  compose (Builder f) (Builder g) = Builder (compose f g)

instance categoryBuilder :: Category Builder where
  identity = Builder identity

insert ::
  forall atom value key o i.
  Atom atom =>
  IsSymbol key =>
  Cons key value i o =>
  atom key value ->
  Builder { | i } { | o }
insert atom = Builder $ Star $ Store.insertAtom atom

build :: forall o i. Builder { | i } { | o } -> Store { | i } -> Effect (Store { | o })
build (Builder (Star runBuilder)) = runBuilder
