module External.Graphics
  ( withGraphics
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad (when)

import Graphics.Rendering.OpenGL (($=))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

withGraphics :: IO () -> IO ()
withGraphics action =
    start >>= flip when (action >> end)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

start :: IO Bool
start = do
    r0 <- GLFW.initialize
    if not r0
      then return False
      else do
          r1 <- GLFW.openWindow GLFW.defaultDisplayOptions
                  { GLFW.displayOptions_width             = 600
                  , GLFW.displayOptions_height            = 600
                  , GLFW.displayOptions_numRedBits        = 5
                  , GLFW.displayOptions_numGreenBits      = 5
                  , GLFW.displayOptions_numBlueBits       = 5
                  , GLFW.displayOptions_numAlphaBits      = 5
                  , GLFW.displayOptions_numFsaaSamples    = Just 8
                  , GLFW.displayOptions_windowIsResizable = True
                  }
          if not r1
            then return False
            else do
                GLFW.setWindowTitle "boomslang"

                GLFW.setWindowSizeCallback $ \w h -> do
                    GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
                    GL.matrixMode $= GL.Projection
                    GL.loadIdentity
                    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

                GL.multisample $= GL.Enabled
                GL.blend       $= GL.Enabled
                GL.blendFunc   $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                GL.clearColor  $= GL.Color4 0.025 0.025 0.025 1

                return True

end :: IO ()
end = do
    GLFW.closeWindow
    GLFW.terminate
