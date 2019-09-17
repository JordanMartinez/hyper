let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.2-20190804/packages.dhall sha256:2230fc547841b54bca815eb0058414aa03ed7b675042f8b3dda644e1952824e5

let overrides =
      { indexed-monad =
              upstream.indexed-monad
          //  { repo =
                  "https://github.com/JordanMartinez/purescript-indexed-monad.git"
              , version =
                  "withMonadInstances"
              }
      }

let additions =
    { indexed-transformers =
        { dependencies =  [ "arrays"
                          , "console"
                          , "control"
                          , "distributive"
                          , "effect"
                          , "either"
                          , "exceptions"
                          , "foldable-traversable"
                          , "indexed-monad"
                          , "identity"
                          , "lazy"
                          , "maybe"
                          , "newtype"
                          , "prelude"
                          , "psci-support"
                          , "tailrec"
                          , "tuples"
                          , "unfoldable"
                          ]
         , repo = "https://github.com/JordanMartinez/purescript-transformers.git"
         , version = "indexed-transformers"
         }
    , indexed-effect =
        { dependencies =
            [ "effect", "prelude", "psci-support", "indexed-monad" ]
        , repo = "https://github.com/JordanMartinez/purescript-indexed-effect.git"
        , version = "master"
        }
    , indexed-aff =
        { dependencies =
            [ "aff", "effect", "prelude", "psci-support", "indexed-monad" ]
        , repo = "https://github.com/JordanMartinez/purescript-indexed-aff.git"
        , version = "master"
        }
    }

in  upstream // overrides // additions
