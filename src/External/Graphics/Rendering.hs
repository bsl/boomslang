module External.Graphics.Rendering (renderScene) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad (unless)
import Data.Char     (ord)
import qualified Data.IntMap as M

import Data.Accessor.Basic ((^.))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Game.Activity   (Activity(..))
import Game.Entity.Dot (Dot)
import Game.G          (G, ask, get, liftIO)
import Game.Score      (Score(..))
import qualified Game.Entity.Dot  as Dot
import qualified Game.Environment as Environment
import qualified Game.Score       as Score
import qualified Game.State       as State
import qualified Vector           as Vector

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderScene :: G ()
renderScene = do
    liftIO $ do
        GL.loadIdentity
        GL.clear [GL.ColorBuffer]

    s <- get
    case s^.State.activity of
      Playing     _ dots        -> mapM_ renderDot dots
      Colliding   _ adots rdots -> mapM_ renderDot adots >>
                                   mapM_ renderDot rdots
      EndingLevel _ dots        -> mapM_ renderDot dots
      _                         -> return ()

    renderStats

    liftIO GLFW.swapBuffers

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderDot :: Dot -> G ()
renderDot d = do
    e <- ask
    liftIO $ do
        GL.loadIdentity
        GL.color c
        GL.translate $ GL.Vector3 (Vector.getX p) (Vector.getY p) 0.0
        GL.scale r r 1
        GL.callList (e^.Environment.discDisplayList)
  where
    c = d^.Dot.color
    p = d^.Dot.position
    r = realToFrac $ d^.Dot.radius :: GL.GLdouble

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderStats :: G ()
renderStats = do
    s <- get
    let sc = s^.State.score
    unless (sc == NoScore) $
      let numDots          = sc^.Score.numDots
          numDotsRequired  = sc^.Score.numDotsRequired
          numDotsActivated = sc^.Score.numDotsActivated
          numPoints        = sc^.Score.numPoints
          orthoMax         = 100 :: GL.GLdouble
          orthoMin         = negate orthoMax
          scoreText        = show numPoints
      in do
        liftIO $ do
            GL.loadIdentity
            GL.ortho2D orthoMin orthoMax orthoMin orthoMax
            GL.translate (GL.Vector3 (orthoMin + 12) (orthoMin + 6) 0)
            GL.color $ GL.Color4 (1 :: GL.GLfloat) 1 1 1
        renderText $ show numDots ++ ":" ++ show numDotsRequired ++ ":" ++
          if numDotsActivated > numDotsRequired
            then show numDotsRequired ++ "+" ++ show (numDotsActivated - numDotsRequired)
            else show numDotsActivated

        -- render Score
        liftIO $ do
            GL.loadIdentity
            GL.ortho2D orthoMin orthoMax orthoMin orthoMax
            let i = 12 + 6 * length scoreText
            GL.translate (GL.Vector3 (orthoMax - fromIntegral i) (orthoMin + 6) 0)
        renderText scoreText

renderText :: String -> G ()
renderText = mapM_ renderChar

renderChar :: Char -> G ()
renderChar c = do
    e <- ask
    liftIO $ maybe
      (return ())
      (\l -> GL.callList l >> GL.translate (GL.Vector3 (6 :: GL.GLdouble) 0 0))
      (M.lookup (ord c) $ e^.Environment.charMap)
