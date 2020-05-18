{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "console"
  , "effect"
  , "filterable"
  , "js-timers"
  , "profunctor"
  , "refs"
  , "strings"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
