module Game.Entity.Dot.Activity
  ( -- * Types
    Activity(..)
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Activity
  = Appearing   !Integer
  | Roaming
  | Expanding   !Integer
  | Holding     !Integer
  | Contracting !Integer
  | Ending      !Integer
  | None
  deriving (Eq, Show)
