module ResponseHelper
  ( fromHTML
  , fromHTML'
  , fromJSON
  , fromJSON'
  , fromStatus
  , status204
  , status301
  , status404
  , status405
  , status500
  ) where

import Prelude

import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (StatusCode)
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Class as Class
import Simple.JSON (class WriteForeign)
import Simple.JSON as SimpleJSON

fromHTML :: String -> Aff Response
fromHTML = fromHTML' StatusCode.status200

fromHTML' :: StatusCode -> String -> Aff Response
fromHTML' status text = do
  body <- Class.liftEffect (Body.toArray text)
  pure
    { body
    , headers: [ Tuple.Tuple "Content-Type" "text/html" ]
    , status
    }

fromJSON :: forall a. WriteForeign a => a -> Aff Response
fromJSON = fromJSON' StatusCode.status200

fromJSON' :: forall a. WriteForeign a => StatusCode -> a -> Aff Response
fromJSON' status json = do
  let text = SimpleJSON.writeJSON json
  body <- Class.liftEffect (Body.toArray text)
  pure
    { body
    , headers: [ Tuple.Tuple "Content-Type" "application/json" ]
    , status
    }

fromStatus :: StatusCode -> Array (Tuple String String) -> Aff Response
fromStatus status headers = do
  let text = SimpleJSON.writeJSON { message: show status }
  body <- Class.liftEffect (Body.toArray text)
  pure
    { body
    , headers: headers <> [ Tuple.Tuple "Content-Type" "application/json" ]
    , status
    }

status204 :: Aff Response
status204 =
  fromStatus StatusCode.status204 []

status301 :: String -> Aff Response
status301 location =
  fromStatus StatusCode.status301 [ Tuple.Tuple "Location" location ]

status404 :: Aff Response
status404 = fromStatus StatusCode.status404 []

status405 :: Array Method -> Aff Response
status405 allow =
  fromStatus
    StatusCode.status405
    [ Tuple.Tuple "Allow" (String.joinWith ", " (map show allow)) ]

status500 :: Aff Response
status500 = fromStatus StatusCode.status500 []
