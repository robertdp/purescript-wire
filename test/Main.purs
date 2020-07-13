module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Wire.Signal as Signal
import Wire.Store as Store
import Wire.Store.Atom (Atom)
import Wire.Store.Atom as Atom
import Wire.Store.Selector (Selector)
import Wire.Store.Selector as Selector

main1 :: Effect Unit
main1 = do
  Console.log "test:"
  let
    signal = do
      a <- pure 1
      b <- pure 2
      c <- pure 3
      pure (a + b + c)
  void $ Signal.subscribe signal Console.logShow

main :: Effect Unit
main = do
  store <-
    Store.empty
      # ( Store.withAtom testAtom1
            >=> Store.withAtom testAtom2
        )
  _ <- Signal.subscribe (Selector.build testSelector store).signal Console.logShow
  launchAff_ do
    delay $ Milliseconds 2000.0
    liftEffect $ Store.updateAtom 4.0 testAtom1 store
    delay $ Milliseconds 5000.0
    liftEffect $ Store.updateAtom 50.0 testAtom2 store

testAtom1 :: Atom "test1" Number
testAtom1 = Atom.makePure 2.0

testAtom2 :: Atom "test2" Number
testAtom2 = Atom.makePure 100.0

testSelector ::
  forall atoms.
  Selector
    ( test1 :: Number
    , test2 :: Number
    | atoms
    )
    Number
testSelector =
  Selector.makeSelector
    { select:
        do
          test1 <- Selector.select testAtom1
          test2 <- Selector.select testAtom2
          pure $ test2 / test1
    , update: mempty
    }
