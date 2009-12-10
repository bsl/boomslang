module Space.Displacement2
  ( -- * Types
    Displacement2
    -- * Constructors
  , make
    -- * Accessors
  , x
  , y
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Template (deriveAccessors)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Displacement2 a = Displacement2
  { x_ :: !a
  , y_ :: !a
  }
  deriving (Eq, Show)

$(deriveAccessors ''Displacement2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

make :: Num a => a -> a -> Displacement2 a
make = Displacement2
