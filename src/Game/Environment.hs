module Game.Environment
  ( -- * Types
    Environment(Environment)
    -- * Accessors
  , secondsPerFrame
  , normalDotRadius
  , normalDotRadiusValue
  , placedDotColor
  , initialDotAlpha
  , minDotDisplacement
  , maxDotDisplacement

  , appearingDotA
  , appearingDotZ
  , appearingDotRadii

  , expandingDotA
  , expandingDotZ
  , expandingDotRadii

  , holdingDotA
  , holdingDotZ
  , holdingDotRadii

  , contractingDotA
  , contractingDotZ
  , contractingDotRadii

  , endingDotA
  , endingDotZ
  , endingDotRadii

    -- * Utilities
  , environment
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Array.Unboxed (UArray, listArray)

import Data.Accessor.Template (deriveAccessors)

import Game.Entity.Dot.Radius (Radius)
import Graphics.Color         (Color)
import qualified Game.Entity.Dot.Radius as Radius
import qualified Graphics.Color         as Color

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Environment = Environment
  { normalDotRadius_      :: !(Radius Double)
  , normalDotRadiusValue_ :: !Double
  , placedDotColor_       :: !(Color Double)
  , initialDotAlpha_      :: !Double
  , minDotDisplacement_   :: !Double
  , maxDotDisplacement_   :: !Double
  , secondsPerFrame_      :: !Double

  , appearingDotA_        :: !Integer
  , appearingDotZ_        :: !Integer
  , appearingDotRadii_    :: !(UArray Integer Double)

  , expandingDotA_        :: !Integer
  , expandingDotZ_        :: !Integer
  , expandingDotRadii_    :: !(UArray Integer Double)

  , holdingDotA_          :: !Integer
  , holdingDotZ_          :: !Integer
  , holdingDotRadii_      :: !(UArray Integer Double)

  , contractingDotA_      :: !Integer
  , contractingDotZ_      :: !Integer
  , contractingDotRadii_  :: !(UArray Integer Double)

  , endingDotA_           :: !Integer
  , endingDotZ_           :: !Integer
  , endingDotRadii_       :: !(UArray Integer Double)
  }
  deriving Show

$(deriveAccessors ''Environment)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

environment :: Environment
environment =
    Environment
      { secondsPerFrame_      = recip (fromIntegral framesPerSecond)
      , normalDotRadius_      = Radius.make normalDotRadiusValue'
      , normalDotRadiusValue_ = normalDotRadiusValue'
      , placedDotColor_       = Color.make (1 :: Double) (1 :: Double) (1 :: Double) initialDotAlpha'
      , initialDotAlpha_      = initialDotAlpha'
      , minDotDisplacement_   = 0.0004 :: Double
      , maxDotDisplacement_   = 0.0025 :: Double

      , appearingDotA_ = appearingDotA'
      , appearingDotZ_ = appearingDotZ'
      , appearingDotRadii_ =
          let maxX = 0.2
              a    = 1
              m    = 100
              d    = log (m * maxX + a) / normalDotRadiusValue'
              xd   = maxX / fromIntegral appearingDotZ'
          in listArray
               (appearingDotA', appearingDotZ')
               [log (m * x + a) / d | x <- tail [0,xd..maxX] :: [Double]]

      , expandingDotA_ = expandingDotA'
      , expandingDotZ_ = expandingDotZ'
      , expandingDotRadii_ =
          let maxX = 0.2
              a    = 1
              m    = 200
              d    = log (m * maxX + a) / (expandedDotRadiusValue' - normalDotRadiusValue')
              xd   = maxX / fromIntegral expandingDotZ'
          in listArray
               (expandingDotA', expandingDotZ')
               [normalDotRadiusValue' + log (m * x + a) / d | x <- tail [0,xd..maxX] :: [Double]]

      , contractingDotA_ = expandingDotA'
      , contractingDotZ_ = expandingDotZ'
      , contractingDotRadii_ =
          let maxX = 0.05
              a    = 1
              m    = 50
              d    = log (m * maxX + a) / expandedDotRadiusValue'
              xd   = maxX / fromIntegral expandingDotZ'
          in listArray
               (expandingDotA', expandingDotZ')
               (reverse [log (m * x + a) / d | x <- tail [0,xd..maxX] :: [Double]])

      , holdingDotA_ = holdingDotA'
      , holdingDotZ_ = holdingDotZ'
      , holdingDotRadii_ =
          let maxX = pi / 8
              xd   = maxX / fromIntegral holdingDotZ'
          in listArray
               (holdingDotA', holdingDotZ')
               (reverse [expandedDotRadiusValue' + (0.005 * sin (40 * x)) | x <- tail [0,xd..maxX] :: [Double]])

      , endingDotA_ = endingDotA'
      , endingDotZ_ = endingDotZ'
      , endingDotRadii_ =
          let maxX = 0.05
              a    = 1
              m    = 50
              d    = log (m * maxX + a) / normalDotRadiusValue'
              xd   = maxX / fromIntegral endingDotZ'
          in listArray
               (endingDotA', endingDotZ')
               (reverse [log (m * x + a) / d | x <- tail [0,xd..maxX] :: [Double]])
      }
  where
    normalDotRadiusValue'   = 0.04 :: Double
    expandedDotRadiusValue' = 0.16 :: Double
    initialDotAlpha'        = 0.6  :: Double
    framesPerSecond         = 150  :: Integer

    appearingDotA' = 1
    appearingDotZ' = 50

    expandingDotA' = 1
    expandingDotZ' = 50

    holdingDotA'   = 1
    holdingDotZ'   = 250

    endingDotA'    = 1
    endingDotZ'    = 30
