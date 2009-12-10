module External.Input.Keyboard
  ( pressed
  , module External.Input.Keyboard.Keys
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import qualified Graphics.UI.GLFW as GLFW

import Game.G (G, liftIO)

import External.Input.Keyboard.Keys

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

pressed :: GLFW.Key -> G Bool
pressed = liftIO . GLFW.keyIsPressed
