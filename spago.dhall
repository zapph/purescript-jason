{ name = "purescript-jason"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-generic"
  , "console"
  , "datetime-iso"
  , "debug"
  , "effect"
  , "js-date"
  , "psci-support"
  , "precise"
  , "precise-datetime"
  , "spec"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
