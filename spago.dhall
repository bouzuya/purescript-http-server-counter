{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "arraybuffer"
    , "bouzuya-datetime-formatter"
    , "bouzuya-http-request-normalized-path"
    , "bouzuya-http-server"
    , "bouzuya-uuid-v4"
    , "node-process"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
