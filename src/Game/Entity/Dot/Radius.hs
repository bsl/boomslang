module Game.Entity.Dot.Radius
  ( -- * Types
    Radius
    -- * Constructors
  , make
    -- * Accessors
  , value
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Exception (assert)

import Data.Accessor.Template (deriveAccessors)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype Radius a = Radius
  { value_ :: a
  }
  deriving (Eq, Show)

$(deriveAccessors ''Radius)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

make :: (Num a, Ord a) => a -> Radius a
make v = Radius (assert (v >= 0) v)
