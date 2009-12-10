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
  = Level1 | Level2 | Level3 | Level4 | Level5
  | Level6 | Level7 | Level8 | Level9 | Level10
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
    let c = (0.1 *) . fromIntegral . fromEnum $ level
        n = numDots level
    in max (ceiling (c * (fromIntegral n :: Double))) 1
