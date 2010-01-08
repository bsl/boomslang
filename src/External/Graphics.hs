module External.Graphics
  ( withGraphics
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Exception (finally)

import Graphics.Rendering.OpenGL (($=))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

withGraphics :: IO a -> IO a
withGraphics action =
    (start >> action) `finally` end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

start :: IO ()
start = do
    GLFW.initialize
    GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_width             = 600
      , GLFW.displayOptions_height            = 600
      , GLFW.displayOptions_numRedBits        = 5
      , GLFW.displayOptions_numGreenBits      = 5
      , GLFW.displayOptions_numBlueBits       = 5
      , GLFW.displayOptions_numAlphaBits      = 5
      , GLFW.displayOptions_numFsaaSamples    = Just 4
      , GLFW.displayOptions_windowIsResizable = True
      }
    GLFW.setWindowTitle "boomslang"

    GLFW.setWindowSizeCallback $ \w h -> do
        GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

    GL.clearColor $= GL.Color4 0.025 0.025 0.025 1

    GL.multisample   $= GL.Enabled
    GL.blend         $= GL.Enabled
    GL.blendFunc     $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.colorMaterial $= Just (GL.Front, GL.AmbientAndDiffuse)
    GL.depthFunc     $= Nothing

    let light0 = GL.Light 0
    GL.ambient  light0 $= GL.Color4 0.3 0.3 0.3 1
    GL.diffuse  light0 $= GL.Color4 0.6 0.6 0.6 1
    GL.specular light0 $= GL.Color4 0.1 0.1 0.1 1
    GL.position light0 $= GL.Vertex4 0 0 2 1
    GL.light    light0 $= GL.Enabled
    GL.lighting        $= GL.Enabled

end :: IO ()
end = do
    GLFW.closeWindow
    GLFW.terminate
