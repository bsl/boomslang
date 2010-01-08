module Game.Entity.Dot
  ( Dot
  , make
  , radius
  , color
  , position
  , direction
  , velocity
  , activity
  , timestamp
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Template (deriveAccessors)

import Game.Entity.Dot.Activity (Activity)
import Vector                   (Vec)
import qualified Graphics.Rendering.OpenGL as GL

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Dot = Dot
  { radius_    :: !Double
  , color_     :: !(GL.Color4 GL.GLfloat)
  , position_  :: !Vec
  , direction_ :: !Vec
  , velocity_  :: !Double
  , activity_  :: !Activity
  , timestamp_ :: !Double
  } deriving (Show, Eq, Ord)

$(deriveAccessors ''Dot)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

make
  :: Double
  -> GL.Color4 GL.GLfloat
  -> Vec
  -> Vec
  -> Double
  -> Activity
  -> Double
  -> Dot
make = Dot
