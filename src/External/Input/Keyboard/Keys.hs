module External.Input.Keyboard.Keys
  ( escape
  , q
  )
  where

import qualified Graphics.UI.GLFW as GLFW

escape :: GLFW.Key
escape = GLFW.KeyEsc

q :: GLFW.Key
q = GLFW.CharKey 'Q'
