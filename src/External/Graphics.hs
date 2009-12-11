module External.Graphics
  ( start
  , end
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Graphics.Rendering.OpenGL (($=))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

start :: IO ()
start = do
    GLFW.initialize
    GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_width             = 600
      , GLFW.displayOptions_height            = 600
      , GLFW.displayOptions_numRedBits        = 8
      , GLFW.displayOptions_numGreenBits      = 8
      , GLFW.displayOptions_numBlueBits       = 8
      , GLFW.displayOptions_numAlphaBits      = 8
      , GLFW.displayOptions_numFsaaSamples    = Just 16
      , GLFW.displayOptions_windowIsResizable = False
      }
    GLFW.setWindowTitle "boomslang"

    GL.multisample   $= GL.Enabled
    GL.blend         $= GL.Enabled
    GL.blendFunc     $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.colorMaterial $= Just (GL.Front, GL.AmbientAndDiffuse)

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
