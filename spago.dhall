{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire"
, dependencies =
  [ "arrays"
  , "avar"
  , "filterable"
  , "js-timers"
  , "profunctor"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
