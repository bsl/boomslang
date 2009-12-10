module Game.Entity.Dot
  ( -- * Types
    Dot
    -- * Constructors
  , make
    -- * Accessors
  , radius
  , color
  , position
  , displacement
  , activity
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Template (deriveAccessors)

import Game.Entity.Dot.Activity (Activity)
import Game.Entity.Dot.Radius   (Radius)
import Graphics.Color           (Color)
import Space.Displacement2      (Displacement2)
import Space.Position2          (Position2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Dot a = Dot
  { radius_       :: !(Radius a)
  , color_        :: !(Color a)
  , position_     :: !(Position2 a)
  , displacement_ :: !(Displacement2 a)
  , activity_     :: !Activity
  }
  deriving (Eq, Show)

$(deriveAccessors ''Dot)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

make :: Radius a -> Color a -> Position2 a -> Displacement2 a -> Activity -> Dot a
make = Dot
