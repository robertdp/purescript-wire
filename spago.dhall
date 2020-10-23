{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire"
, license = "BSD-3-Clause"
, repository = "https://github.com/robertdp/purescript-wire.git"
, dependencies =
  [ "aff"
  , "filterable"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
