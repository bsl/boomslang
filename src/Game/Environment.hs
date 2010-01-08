module Game.Environment
  ( Environment (Environment)
  , normalDotRadius
  , placedDotColor
  , dotVelocity
  , framesPerSecond
  , discDisplayList
  , charMap
  , environment
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad          (forM)
import Data.Char              (ord)
import qualified Data.IntMap as M

import Data.Accessor.Template (deriveAccessors)

import qualified Graphics.Fonts.OpenGL.Basic4x6 as Basic4x6
import qualified Graphics.Rendering.OpenGL      as GL

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Environment = Environment
  { normalDotRadius_ :: !Double
  , placedDotColor_  :: !(GL.Color4 GL.GLfloat)
  , dotVelocity_     :: !Double
  , framesPerSecond_ :: !Double
  , discDisplayList_ :: !GL.DisplayList
  , charMap_         :: !(M.IntMap GL.DisplayList)
  }

$(deriveAccessors ''Environment)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

disc :: IO ()
disc =
    GL.renderPrimitive GL.TriangleFan $
      mapM_ GL.vertex (centralVertex : outerVertices)
  where
    centralVertex = GL.Vertex2 0 0
    outerVertices = [ GL.Vertex2 (cos r) (sin r) | r <- [0::GL.GLdouble,2*pi/360*12..2*pi] ]

environment :: IO Environment
environment = do
    discDisplayList' <- GL.defineNewList GL.Compile disc
    charMap' <- charToDisplayList
    return Environment
      { normalDotRadius_   = normalDotRadius'
      , placedDotColor_    = translucentWhite
      , dotVelocity_       = 0.2 / framesPerSecond'
      , framesPerSecond_   = framesPerSecond'
      , discDisplayList_   = discDisplayList'
      , charMap_           = charMap'
      }
  where
    normalDotRadius' = 0.04 :: Double
    framesPerSecond' = 60   :: Double

translucentWhite :: GL.Color4 GL.GLfloat
translucentWhite = GL.Color4 1 1 1 0.6

charToDisplayList :: IO (M.IntMap GL.DisplayList)
charToDisplayList =
    M.fromList `fmap` charDisplayLists

charDisplayLists :: IO [(Int,GL.DisplayList)]
charDisplayLists =
    forM chars $ \(c,f) -> do
        dl <- GL.defineNewList GL.Compile f
        return (ord c,dl)

chars :: [(Char,IO ())]
chars =
    [ ('0', Basic4x6.digit0)
    , ('1', Basic4x6.digit1)
    , ('2', Basic4x6.digit2)
    , ('3', Basic4x6.digit3)
    , ('4', Basic4x6.digit4)
    , ('5', Basic4x6.digit5)
    , ('6', Basic4x6.digit6)
    , ('7', Basic4x6.digit7)
    , ('8', Basic4x6.digit8)
    , ('9', Basic4x6.digit9)
    , ('!', Basic4x6.exclamation)
    , ('%', Basic4x6.percent)
    , ('+', Basic4x6.plus)
    , ('-', Basic4x6.minus)
    , ('.', Basic4x6.period)
    , ('/', Basic4x6.slash)
    , (':', Basic4x6.colon)
    , ('<', Basic4x6.lessThan)
    , ('=', Basic4x6.equal)
    , ('>', Basic4x6.greaterThan)
    ]
