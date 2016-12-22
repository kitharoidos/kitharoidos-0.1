-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNCanvas
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

module Kitharoidos.GFNNCanvas (
  CanvasPars (CanvasPars, canvasSize, canvasColor),
  runGFNNCanvas
) where

import Kitharoidos.GFNNCanvas.CanvasPars
import Kitharoidos.GFNNCanvas.InitCanvas
import Kitharoidos.GFNNCanvas.DisplayCanvas
import Kitharoidos.GFNNCanvas.DisplayGFNN
import Kitharoidos.GFNNCanvas.ScrapCanvas
import Kitharoidos.GFNNCanvas.TerminateCanvas
import Control.Monad
import Control.Exception

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Run canvas for rendering GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runGFNNCanvas :: CanvasPars -> IO (IO ()) -> IO ()
runGFNNCanvas canvasPars receiveRendering
  = bracket_ (bracketOnError initCanvas (const terminateCanvas) (const $ displayCanvas canvasPars))
             (scrapCanvas >> terminateCanvas)
             (do staves <- receiveRendering
                 forever $ receiveRendering >>= displayGFNN staves
             )
