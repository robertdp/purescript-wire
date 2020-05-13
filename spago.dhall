{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire-signal"
, dependencies =
  [ "arrays"
  , "effect"
  , "filterable"
  , "profunctor"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
