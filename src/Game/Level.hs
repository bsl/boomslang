module Game.Level
  ( -- * Types
    Level(..)
    -- * Utilities
  , number
  , next
  , numDots
  , numDotsRequired
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Level
  = Level1 | Level2 | Level3 | Level4  | Level5  | Level6
  | Level7 | Level8 | Level9 | Level10 | Level11 | Level12
  deriving (Bounded, Enum, Eq, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

number :: Level -> Integer
number = succ . fromIntegral . fromEnum

next :: Level -> Maybe Level
next c =
    if c == maxLevel
      then Nothing
      else (Just . toEnum . succ . fromEnum) c
  where
    maxLevel = maxBound :: Level

numDots :: Level -> Integer
numDots = (5 *) . succ . fromIntegral . fromEnum

numDotsRequired :: Level -> Integer
numDotsRequired level =
    case level of
      Level1  -> 1
      Level2  -> 2
      Level3  -> 3
      Level4  -> 5
      Level5  -> 7
      Level6  -> 10
      Level7  -> 15
      Level8  -> 21
      Level9  -> 27
      Level10 -> 33
      Level11 -> 44
      Level12 -> 55
