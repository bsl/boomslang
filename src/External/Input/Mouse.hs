module External.Input.Mouse
  ( clicked
  , module External.Input.Mouse.Buttons
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Concurrent.MVar (readMVar)

import Data.Accessor.Basic ((^.))

import qualified Graphics.UI.GLFW as GLFW

import Game.G (G, get, liftIO)
import qualified Game.State as State

import External.Input.Mouse.Buttons

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

clicked :: GLFW.MouseButton -> G (Maybe (Double,Double))
clicked b = do
    s <- get
    liftIO $ do
        r <- GLFW.mouseButtonIsPressed b
        if r
          then do
              (x, y) <- GLFW.getMousePosition
              (w,h) <- readMVar (s^.State.windowSize)
              let w2 = fromIntegral w / 2
              let h2 = fromIntegral h / 2
              let x' = (fromIntegral x - w2) / w2
              let y' = (h2 - fromIntegral y) / h2
              return $ Just (x',y')
          else return Nothing
