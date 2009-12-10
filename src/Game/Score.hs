module Game.Score
  ( -- * Types
    Score(..)
    -- * Accessors
  , numDots
  , numDotsRequired
  , numDotsActivated
  , numPoints
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Template (deriveAccessors)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Score
  = NoScore
  | Score
      { numDots_          :: !Integer
      , numDotsRequired_  :: !Integer
      , numDotsActivated_ :: !Integer
      , numPoints_        :: !Integer
      }
  deriving (Eq, Show)

$(deriveAccessors ''Score)
