module Store
  ( Store
  , delete
  , empty
  , get
  , insert
  , list
  , update
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Id = String
newtype Store a = Store (Ref (Array (Tuple Id a)))

delete :: forall a. Store a -> Id -> Aff Unit
delete (Store ref) id = Class.liftEffect do
  Ref.modify_ (Array.filter ((notEq id) <<< Tuple.fst)) ref

empty :: forall a. Aff (Store a)
empty = Class.liftEffect (map Store (Ref.new []))

get :: forall a. Store a -> Id -> Aff (Maybe a)
get (Store ref) id = Class.liftEffect do
  xs <- Ref.read ref
  pure (map Tuple.snd (Array.find ((eq id) <<< Tuple.fst) xs))

insert :: forall a. Store a -> Id -> a -> Aff Unit
insert (Store ref) id x = Class.liftEffect do
  xs <- Ref.read ref
  Ref.write (Array.snoc xs (Tuple.Tuple id x)) ref

list :: forall a. Store a -> Aff (Array a)
list (Store ref) = Class.liftEffect (map (map Tuple.snd) (Ref.read ref))

update :: forall a. Store a -> Id -> a -> Aff Unit
update (Store ref) id x = Class.liftEffect do
  xs <- Ref.read ref
  Maybe.maybe (pure unit) (flip Ref.write ref) do
    index <- Array.findIndex ((eq id) <<< Tuple.fst) xs
    Array.updateAt index (Tuple.Tuple id x) xs
