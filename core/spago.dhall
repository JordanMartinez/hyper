{ sources =
    [ "src/**/*.purs" ]
, name =
    "hyper-core"
, dependencies =
    [ "aff"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "generics-rep"
    , "http-methods"
    , "indexed-aff"
    , "indexed-effect"
    , "indexed-monad"
    , "indexed-transformers"
    , "media-types"
    , "profunctor-lenses"
    , "proxy"
    , "psci-support"
    , "record-extra"
    , "strings"
    , "transformers"
    ]
, packages =
    ../packages.dhall
}
