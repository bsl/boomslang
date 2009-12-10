module External.Graphics.Rendering (renderScene) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad (unless)
import Data.List     (unfoldr)

import Data.Accessor.Basic ((^.))
import qualified Graphics.Fonts.OpenGL.Basic4x6 as Basic4x6
import qualified Graphics.Rendering.OpenGL      as GL
import qualified Graphics.UI.GLFW               as GLFW

import Game.Activity   (Activity(..))
import Game.Entity.Dot (Dot)
import Game.G          (G, get, liftIO)
import Game.Score      (Score(..))
import qualified Game.Entity.Dot        as Dot
import qualified Game.Entity.Dot.Radius as Radius
import qualified Game.Score             as Score
import qualified Game.State             as State
import qualified Graphics.Color         as Color
import qualified Space.Position2        as Position2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderScene :: G ()
renderScene = do
    liftIO $ do
        GL.loadIdentity
        GL.clear [GL.ColorBuffer]

    s <- get
    case s^.State.activity of
      Playing         _ dots        -> mapM_ renderDot dots
      Colliding       _ adots rdots -> mapM_ renderDot adots >>
                                       mapM_ renderDot rdots
      EndingLevel     _ dots        -> mapM_ renderDot dots
      _                             -> return ()

    renderScore

    liftIO GLFW.swapBuffers

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderDot :: Dot Double -> G ()
renderDot d =
    liftIO $ do
        GL.color (GL.Color4 cr cg cb ca)
        GL.renderPrimitive GL.TriangleFan $
          mapM_ GL.vertex (centralVertex : outerVertices)
  where
    centralVertex = GL.Vertex2 px py

    outerVertices =
      [ GL.Vertex2 (px + x) (py + y)
      | (x,y) <- take (succ n) $
          iterate (\(x,y) ->
            (cost * x - sint * y, sint * x + cost * y))
            (0 :: GL.GLdouble, realToFrac r)
      ]

    cr = realToFrac $ c^.Color.redChannel   :: GL.GLdouble
    cg = realToFrac $ c^.Color.greenChannel :: GL.GLdouble
    cb = realToFrac $ c^.Color.blueChannel  :: GL.GLdouble
    ca = realToFrac $ c^.Color.alphaChannel :: GL.GLdouble
    c  = d^.Dot.color

    px = realToFrac $ p^.Position2.x
    py = realToFrac $ p^.Position2.y
    p  = d^.Dot.position

    cost = cos t
    sint = sin t
    t    = pi2 / fromIntegral n
    pi2  = 2 * pi
    n    = 50  -- TODO: calculate number of segments based on radius

    r  = d^.Dot.radius^.Radius.value

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderScore :: G ()
renderScore = do
    s <- get
    let sc = s^.State.score
    unless (sc == NoScore) $
      let numDots          = sc^.Score.numDots
          numDotsRequired  = sc^.Score.numDotsRequired
          numDotsActivated = sc^.Score.numDotsActivated
          numPoints        = sc^.Score.numPoints
      in
        liftIO $ do
            GL.loadIdentity
            GL.ortho2D orthoMin orthoMax orthoMin orthoMax
            GL.translate (GL.Vector3 (orthoMin + 12) (orthoMin + 6) 0)

            renderNumber'    numDots
            renderCharacter' Basic4x6.colon
            renderNumber'    numDotsRequired
            renderCharacter' Basic4x6.colon

            if numDotsActivated > numDotsRequired
              then do
                  renderNumber'    numDotsRequired
                  renderCharacter' Basic4x6.plus
                  renderNumber'    (numDotsActivated - numDotsRequired)
              else renderNumber' numDotsActivated

            GL.loadIdentity
            GL.ortho2D orthoMin orthoMax orthoMin orthoMax
            let i = 12 + 6 * numDigits numPoints
            GL.translate (GL.Vector3 (orthoMax - fromIntegral i) (orthoMin + 6) 0)
            renderNumber' numPoints
          where
            orthoMax = 100 :: GL.GLdouble
            orthoMin = negate orthoMax

            renderNumber' n = do
                GL.color $ GL.Color4 (1 :: GL.GLfloat) 1 1 1
                renderNumber n

            renderCharacter' c = do
                GL.color $ GL.Color4 (0.4 :: GL.GLfloat) 0.6 0.4 1
                renderCharacter c

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderNumber :: Integer -> IO ()
renderNumber = mapM_ renderDigit . integerToDigits

renderDigit :: Integer -> IO ()
renderDigit d =
    renderCharacter c
  where
    c = case d of
      0 -> Basic4x6.digit0
      1 -> Basic4x6.digit1
      2 -> Basic4x6.digit2
      3 -> Basic4x6.digit3
      4 -> Basic4x6.digit4
      5 -> Basic4x6.digit5
      6 -> Basic4x6.digit6
      7 -> Basic4x6.digit7
      8 -> Basic4x6.digit8
      9 -> Basic4x6.digit9
      _ -> return ()

integerToDigits :: Integer -> [Integer]
integerToDigits n =
    let ds = reverse (unfoldr f n)
    in if null ds then [0] else ds
  where
    f i = if i == 0 then Nothing else Just (mod i 10, div i 10)

numDigits :: Integer -> Integer
numDigits 0 = 1
numDigits n =
    let n' = fromIntegral n :: Double
    in succ . truncate . logBase 10 . abs $ n'

renderCharacter :: IO () -> IO ()
renderCharacter = (>> GL.translate (GL.Vector3 (6 :: GL.GLdouble) 0 0))
