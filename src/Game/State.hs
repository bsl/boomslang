module Game.State
  ( State (State)
  , activity
  , score
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Template (deriveAccessors)

import Game.Activity (Activity)
import Game.Score    (Score)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data State = State
  { activity_ :: !Activity
  , score_    :: !Score
  }

$(deriveAccessors ''State)
