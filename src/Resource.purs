module Resource
  ( Counter
  , CounterId
  , CounterParams
  ) where

type Counter =
  { count :: Int
  , id :: CounterId
  , name :: String
  , created_at :: String
  }

type CounterId = String

type CounterParams =
  { name :: String
  }
