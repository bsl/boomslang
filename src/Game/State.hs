module Game.State
  ( -- * Types
    State(State)
    -- * Accessors
  , windowSize
  , activity
  , score
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Concurrent.MVar (MVar)

import Data.Accessor.Template (deriveAccessors)

import Game.Activity (Activity)
import Game.Score    (Score)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data State = State
  { windowSize_ :: MVar (Integer,Integer)
  , activity_   :: !Activity
  , score_      :: !Score
  }

$(deriveAccessors ''State)
