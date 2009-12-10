module Space.Position2
  ( -- * Types
    Position2
    -- * Constructors
  , make
    -- * Accessors
  , x
  , y
    -- * Utilities
  , displace
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Basic    ((^.))
import Data.Accessor.Template (deriveAccessors)

import Space.Displacement2 (Displacement2)
import qualified Space.Displacement2 as D

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Position2 a = Position2
  { x_ :: !a
  , y_ :: !a
  }
  deriving (Eq, Show)

$(deriveAccessors ''Position2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

make :: Num a => a -> a -> Position2 a
make = Position2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

displace :: Num a => Position2 a -> Displacement2 a -> Position2 a
displace p d =
    make px' py'
  where
    px' = p^.x + d^.D.x
    py' = p^.y + d^.D.y
