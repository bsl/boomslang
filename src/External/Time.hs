module External.Time
  ( resetTimer
  , readTimer
  , sleep
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import qualified Graphics.UI.GLFW as GLFW

import Game.G (G, liftIO)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

resetTimer :: G ()
resetTimer = liftIO GLFW.resetTime

readTimer :: G Double
readTimer = liftIO GLFW.getTime

sleep :: Double -> G ()
sleep = liftIO . GLFW.sleep
