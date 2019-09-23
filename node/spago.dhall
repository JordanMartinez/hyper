{ sources =
    [ "src/**/*.purs" ]
, name =
    "hyper-node"
, dependencies =
[ "aff"
, "avar"
, "control"
, "effect"
, "http-methods"
, "hyper-core"
, "node-buffer"
, "node-fs-aff"
, "node-http"
, "psci-support"
, "strings"
, "transformers"
]
, packages =
    ../packages.dhall
}
