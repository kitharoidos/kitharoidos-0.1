module Kitharoidos.GFNNCanvas.InitCanvas (
  initCanvas
) where

import Kitharoidos.GFNNCanvas.CanvasError
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Exception
import Control.Monad

-- | Initialize canvas for rendering GFNN.
initCanvas :: IO ()
initCanvas = initialize >>= (\result -> unless result (throwIO GLFWInitializationError))
