module Kitharoidos.GFNNCanvas.CanvasPars (
  CanvasPars (CanvasPars, canvasSize, canvasColor)
) where

import Graphics.Rendering.OpenGL

-- | Parameters of canvas for rendering GFNN.
data CanvasPars = CanvasPars { canvasSize  :: Size
                             , canvasColor :: Color4 GLclampf
                             }
