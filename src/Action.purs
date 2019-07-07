module Action
  ( Action(..)
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method)
import Data.String as String
import Resource (CounterId)

data Action
  = HealthCheck
  | MethodNotAllowed (Array Method)
  | NotFound
  | CounterCreate
  | CounterDelete CounterId
  | CounterGet CounterId
  | CounterList
  | CounterUpdate CounterId

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    HealthCheck -> "HealthCheck"
    (MethodNotAllowed methods) ->
      "MethodNotAllowed " <> (String.joinWith ", " (map show methods))
    NotFound -> "NotFound "
    CounterCreate -> "CounterCreate"
    CounterDelete id -> "CounterDelete " <> id
    CounterGet id -> "CounterGet " <> id
    CounterList -> "CounterList"
    CounterUpdate id -> "CounterId " <> id
