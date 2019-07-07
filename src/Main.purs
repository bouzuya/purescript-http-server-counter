module Main
  ( main
  ) where

import Prelude

import ActionHandler as ActionHandler
import Bouzuya.HTTP.Server as Server
import Control.Bind (bindFlipped)
import Data.Int as Int
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Console as Console
import Node.Process as Process
import Store as Store

readPort :: Int -> Effect Int
readPort defaultPort =
  map
    (Maybe.fromMaybe defaultPort)
    (map (bindFlipped Int.fromString) (Process.lookupEnv "PORT"))

main :: Effect Unit
main = Aff.launchAff_ do
  store <- Store.new []
  port <- Class.liftEffect (readPort 8080)
  Class.liftEffect
    (Server.run
      { host: "0.0.0.0", port }
      logListen
      (ActionHandler.handler store))

logListen :: { host :: String, port :: Int } -> Effect Unit
logListen { host, port } =
  Console.log ("listening on http://" <> host <> ":" <> show port)
