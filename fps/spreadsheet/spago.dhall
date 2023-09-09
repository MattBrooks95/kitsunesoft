{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "fsp - spreadsheets"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "lists"
  , "matrices"
  , "maybe"
  , "milkis"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
