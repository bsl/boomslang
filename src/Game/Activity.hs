module Game.Activity
  ( Activity (..)
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Game.Entity.Dot (Dot)
import Game.Level      (Level)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Activity
  = Starting
  | PreparingLevel !Level
  | Playing        !Level ![Dot]
  | Colliding      !Level ![Dot] ![Dot]
  | EndingLevel    !Level ![Dot]
  | Quitting
