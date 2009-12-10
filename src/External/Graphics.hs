module External.Graphics
  ( start
  , end
  , adjustAfterWindowResize
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

start :: IO (MVar (Integer,Integer))
start = do
    GLFW.initialize
    GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_width          = 600
      , GLFW.displayOptions_height         = 600
      , GLFW.displayOptions_numRedBits     = 8
      , GLFW.displayOptions_numGreenBits   = 8
      , GLFW.displayOptions_numBlueBits    = 8
      , GLFW.displayOptions_numAlphaBits   = 8
      , GLFW.displayOptions_numFsaaSamples = Just 16
      }
    GLFW.setWindowTitle "boomslang"

    GLFW.getWindowValue GLFW.NumFsaaSamples >>= print

    windowSize <- newEmptyMVar
    GLFW.setWindowSizeCallback (windowSizeCallback windowSize)

    GL.multisample           $= GL.Enabled

    GL.blend                 $= GL.Enabled
    GL.blendFunc             $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    GL.colorMaterial         $= Just (GL.Front, GL.AmbientAndDiffuse)
    -- GL.shadeModel            $= GL.Smooth

    let light0 = GL.Light 0
    GL.ambient  light0 $= GL.Color4 0.3 0.3 0.3 1
    GL.diffuse  light0 $= GL.Color4 0.6 0.6 0.6 1
    GL.specular light0 $= GL.Color4 0.1 0.1 0.1 1
    GL.position light0 $= GL.Vertex4 0 0 2 1
    GL.light    light0 $= GL.Enabled
    GL.lighting        $= GL.Enabled

    return windowSize

end :: IO ()
end = do
    GLFW.closeWindow
    GLFW.terminate

adjustAfterWindowResize :: Integer -> Integer -> IO ()
adjustAfterWindowResize w h = do
    GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

windowSizeCallback :: MVar (Integer,Integer) -> Int -> Int -> IO ()
windowSizeCallback windowSize w h = do
    _ <- tryTakeMVar windowSize
    putMVar windowSize (fromIntegral w, fromIntegral h)
