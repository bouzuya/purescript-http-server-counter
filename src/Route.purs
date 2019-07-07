module Route
  ( route
  ) where

import Action (Action)
import Action as Action
import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath (NormalizedPath)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath

route :: NormalizedPath -> Method -> Action
route normalizedPath method =
  case NormalizedPath.toPieces normalizedPath of
    ["counters"] ->
      case method of
        Method.GET -> Action.CounterList
        Method.POST -> Action.CounterCreate
        _ -> Action.MethodNotAllowed [Method.GET, Method.POST]
    ["counters", id] ->
      case method of
        Method.GET -> Action.CounterGet id
        Method.DELETE -> Action.CounterDelete id
        Method.PATCH -> Action.CounterUpdate id
        _ -> Action.MethodNotAllowed [Method.GET, Method.DELETE, Method.PATCH]
    [] ->
      case method of
        Method.GET -> Action.HealthCheck
        _ -> Action.MethodNotAllowed [Method.GET]
    _ -> Action.NotFound
