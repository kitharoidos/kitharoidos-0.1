module Kitharoidos.GFNNCanvas.TerminateCanvas (
  terminateCanvas
) where

import Graphics.UI.GLFW

-- | Terminate canvas for rendering GFNN.
terminateCanvas :: IO ()
terminateCanvas = terminate
