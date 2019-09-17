{ sources =
    [ "src/**/*.purs" ]
, name =
    "hyper"
, dependencies =
    [ "aff"
    , "argonaut"
    , "arrays"
    , "avar"
    , "console"
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
    , "node-buffer"
    , "node-fs-aff"
    , "node-http"
    , "ordered-collections"
    , "proxy"
    , "psci-support"
    , "random"
    , "record-extra"
    , "smolder"
    , "spec"
    , "spec-discovery"
    , "strings"
    , "transformers"
    ]
, packages =
    ./packages.dhall
}
