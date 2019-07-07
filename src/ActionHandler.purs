module ActionHandler
  ( handler
  ) where

import Prelude

import Action as Action
import Bouzuya.DateTime.Formatter.DateTime as DateTimeFormatter
import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode as StatusCode
import Bouzuya.UUID.V4 as UUIDv4
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Class.Console as Console
import Effect.Now as Now
import Partial.Unsafe as Unsafe
import Record as Record
import Resource (Counter, CounterParams, CounterId)
import ResponseHelper as ResponseHelper
import Route as Route
import Simple.JSON as SimpleJSON
import Store (Store)
import Store as Store

handler :: Store (Array Counter) -> Request -> Aff Response
handler store { body, method, pathname } = do
  Console.log ((show method) <> " " <> pathname)
  let
    normalized = NormalizedPath.normalize pathname
    normalizedPathname = NormalizedPath.toString normalized
  if pathname /= normalizedPathname
    then ResponseHelper.status301 normalizedPathname
    else do
      let action = Route.route normalized method
      case action of
        Action.CounterCreate -> do
          body' <- Class.liftEffect (Body.fromArray body) -- TODO
          case (SimpleJSON.readJSON_ body' :: _ CounterParams) of
            Maybe.Nothing ->
              -- TODO: message
              ResponseHelper.fromStatus StatusCode.status400 []
            Maybe.Just params -> do
              counter <- create store params
              ResponseHelper.fromJSON counter
        Action.CounterDelete id -> do
          _ <- delete store id
          ResponseHelper.status204
        Action.CounterGet id -> do
          counterMaybe <- get store id
          case counterMaybe of
            Maybe.Nothing -> ResponseHelper.status404
            Maybe.Just counter -> ResponseHelper.fromJSON counter
        Action.CounterList -> do
          counters <- list store
          ResponseHelper.fromJSON counters
        Action.CounterUpdate id -> do
          counterMaybe <- update store id
          case counterMaybe of
            Maybe.Nothing -> ResponseHelper.status404
            Maybe.Just counter -> ResponseHelper.fromJSON counter
        Action.HealthCheck ->
          ResponseHelper.fromStatus StatusCode.status200 []
        Action.MethodNotAllowed methods ->
          ResponseHelper.status405 methods
        Action.NotFound ->
          ResponseHelper.status404

create :: Store (Array Counter) -> CounterParams -> Aff Counter
create store params = do
  id <- Class.liftEffect (map UUIDv4.toString UUIDv4.generate)
  created_at <- Class.liftEffect (map dateTimeToString Now.nowDateTime)
  let counter = Record.merge params { count: 0, created_at, id }
  counters <- Store.get store
  let counters' = Array.insert counter counters
  _ <- Store.put counters' store
  pure counter

delete :: Store (Array Counter) -> CounterId -> Aff (Maybe Unit)
delete store id = do
  counters <- Store.get store
  case Array.findIndex ((eq id) <<< _.id) counters of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index -> do
      case Array.deleteAt index counters of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just counters' -> do
          _ <- Store.put counters' store
          pure (Maybe.Just unit)

get :: Store (Array Counter) -> CounterId -> Aff (Maybe Counter)
get store id = do
  counters <- Store.get store
  pure (Array.find ((eq id) <<< _.id) counters)

list :: Store (Array Counter) -> Aff (Array Counter)
list = Store.get

update :: Store (Array Counter) -> CounterId -> Aff (Maybe Counter)
update store id = do
  counters <- Store.get store
  case Array.findIndex ((eq id) <<< _.id) counters of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index -> do
      let
        counter = Unsafe.unsafePartial (Array.unsafeIndex counters index)
        counter' = counter { count = counter.count + 1 }
      case Array.updateAt index counter' counters of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just counters' -> do
          _ <- Store.put counters' store
          pure (Maybe.Just counter')

dateTimeToString :: DateTime -> String
dateTimeToString dt = DateTimeFormatter.toString dt <> "Z"
