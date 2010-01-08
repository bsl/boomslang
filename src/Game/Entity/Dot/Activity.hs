module Game.Entity.Dot.Activity
  ( Activity (..)
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Activity
  = Roaming
  | Hit
  | None
  deriving (Eq, Show, Ord)
