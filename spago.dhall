{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire"
, dependencies =
  [ "arrays"
  , "filterable"
  , "foreign-object"
  , "freet"
  , "profunctor"
  , "react-basic-hooks"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
