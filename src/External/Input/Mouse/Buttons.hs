module External.Input.Mouse.Buttons
  ( left
  , right
  ) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Graphics.UI.GLFW (MouseButton(..))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

left :: MouseButton
left = MouseButton0

right :: MouseButton
right = MouseButton1
