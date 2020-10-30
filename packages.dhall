
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200911-2/packages.dhall sha256:872c06349ed9c8210be43982dc6466c2ca7c5c441129826bcb9bf3672938f16e

let overrides = {=}

let additions =
  { datetime-iso =
    { dependencies =
        [ "newtype"
        , "parsing"
        , "argonaut-codecs"
        , "datetime"
        ]
        , repo = "https://github.com/seanyu4296/purescript-datetime-iso.git"
        , version = "a00d251c08f7c6bbcda0a0ecde9692d4afaaf2e5"
    }
  , precise =
    { dependencies =
      [ "arrays"
      , "globals"
      , "integers"
      , "generics-rep"
      , "strings"
      , "gen"
      , "lists"
      , "exceptions"
      ]
    , repo =
        "https://github.com/purescript-contrib/purescript-precise.git"
    , version = "v4.0.0"
    }
  }

in  upstream // overrides // additions