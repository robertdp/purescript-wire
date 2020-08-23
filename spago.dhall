{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire"
, dependencies =
  [ "aff"
  , "arrays"
  , "filterable"
  , "foreign-object"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
