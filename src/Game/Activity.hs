module Game.Activity
  ( -- * Types
    Activity(..)
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Game.Entity.Dot (Dot)
import Game.Level      (Level)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Activity
  = Starting
  | PreparingLevel !Level
  | Playing        !Level ![Dot Double]
  | Colliding      !Level ![Dot Double] ![Dot Double]
  | EndingLevel    !Level ![Dot Double]
  | Quitting
  deriving (Eq, Show)
