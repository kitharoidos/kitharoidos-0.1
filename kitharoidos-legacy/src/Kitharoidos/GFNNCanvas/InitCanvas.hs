-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNCanvas.InitCanvas
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Kitharoidos.GFNNCanvas.InitCanvas (
  initCanvas
) where

import Kitharoidos.GFNNCanvas.CanvasError
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Exception

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Initialize canvas for rendering GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
initCanvas :: IO ()
initCanvas = initialize >>= (\result -> if result then return () else throwIO GLFWInitializationError)
