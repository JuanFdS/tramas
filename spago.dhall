{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "tramas"
, dependencies =
    [ "effect", "console", "psci-support", "react-basic", "quickcheck" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
