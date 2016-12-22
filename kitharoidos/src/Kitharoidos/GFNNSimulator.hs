-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNSimulator
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

module Kitharoidos.GFNNSimulator (
  SimulatorPars (SimulatorPars, oscLPars, connLPars, connKernel, gfnnDt),
  View (View, normAvgRZs, instFreqs, normAvgRCs),
  runGFNNSimulator, oscEq, connEq
) where

import Kitharoidos.GFNNSimulator.SimulatorPars
import Kitharoidos.GFNNArchitect (Architecture)
import Kitharoidos.GFNNSimulator.InitSimulator
import Kitharoidos.GFNNSimulator.GFNN
import Kitharoidos.GFNNSimulator.RunGFNN
import Kitharoidos.GFNNSimulator.ViewGFNN
import Kitharoidos.GFNNSimulator.View
import Control.ContStuff
import Control.Monad
import qualified Data.Vector.Unboxed as V
import Data.Complex

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Run GFNN simulator
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runGFNNSimulator :: Architecture -> SimulatorPars ->
                    IO (V.Vector (Double, Double)) -> (View -> IO ()) -> IO ()
runGFNNSimulator architecture simulatorPars readInputs writeView
  = evalStateT emptyGFNN $ do
      initSimulator architecture simulatorPars
      forever $ do
        liftIO readInputs >>= V.mapM runGFNN >>=
          \results -> case V.length results of
            0 -> return ()
            _ -> viewGFNN >>= (liftIO . writeView)
