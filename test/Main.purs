module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Wire.React.Class as Atom
import Wire.React.Pure as Pure
import Wire.React.Selector as Selector

main :: Effect Unit
main = do
  testAtom1 <- Pure.create 2.0
  testAtom2 <- Pure.create 100.0
  testSelector <-
    Selector.create
      { select:
          do
            test1 <- Selector.select testAtom1
            test2 <- Selector.select testAtom2
            pure $ test2 / test1
      , update: mempty
      }
  _ <- Atom.subscribe testSelector Console.logShow
  launchAff_ do
    delay $ Milliseconds 2000.0
    liftEffect $ Atom.modify testAtom1 (const 4.0)
    delay $ Milliseconds 5000.0
    liftEffect $ Atom.modify testAtom2 (const 50.0)
