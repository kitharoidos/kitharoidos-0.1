-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNSimulator.SimulatorPars
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

{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNSimulator.SimulatorPars (
  SimulatorPars (SimulatorPars, oscLPars, connLPars, connKernel, gfnnDt)
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import Data.Complex

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Parameters for GFNN simulator
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data SimulatorPars = SimulatorPars { oscLPars   :: !(G.Vector (Double, V.Vector (Double, Double), Double))

                                   , connLPars  :: !(G.Vector (Double, Double, Double))
                                   , connKernel :: !(Int -> Double -> Double -> Double -> (Double, Double))

                                   , gfnnDt     :: !Double
                                   }

instance Show SimulatorPars where
  show SimulatorPars {oscLPars, connLPars, gfnnDt}
    = "SimulatorPars:\n\n" ++
      "  oscLPars: "  ++ show (G.map (\(alpha, betasDeltas, epsilon) ->
                                        (alpha, V.toList betasDeltas, epsilon)
                                     )
                                     oscLPars
                              )         ++ "\n\n" ++
      "  connLPars: " ++ show connLPars ++ "\n\n" ++
      "  gfnnDt: "    ++ show gfnnDt ++ "\n\n\n"

