module Kitharoidos.GFNNCanvas.DisplayGFNN (
  displayGFNN
) where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Monad

-- | Render GFNN on the canvas.
displayGFNN :: IO () -> IO () -> IO ()
displayGFNN staves rendering = do
  clear [ColorBuffer]
  staves >> rendering
  swapBuffers
