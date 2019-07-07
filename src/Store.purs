module Store
  ( Store
  , get
  , new
  , put
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype Store a = Store (Ref a)

get :: forall a. Store a -> Aff a
get (Store ref) = Class.liftEffect (Ref.read ref)

new :: forall a. a -> Aff (Store a)
new x = Class.liftEffect (map Store (Ref.new x))

put :: forall a. a -> Store a -> Aff Unit
put x (Store ref) = Class.liftEffect (Ref.write x ref)
