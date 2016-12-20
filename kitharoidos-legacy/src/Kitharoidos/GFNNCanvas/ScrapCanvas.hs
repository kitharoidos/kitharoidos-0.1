-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNCanvas.ScrapCanvas
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

module Kitharoidos.GFNNCanvas.ScrapCanvas (
  scrapCanvas
) where

import Graphics.UI.GLFW

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Scrap canvas for rendering GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
scrapCanvas :: IO ()
scrapCanvas = closeWindow
