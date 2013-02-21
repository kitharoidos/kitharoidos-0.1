-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.InputGenerator.GenerateInputs
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

module Kitharoidos.InputGenerator.GenerateInputs (
  generateInputs, r, phi
) where

import Kitharoidos.InputGenerator.GeneratorState
import Control.ContStuff
import qualified Data.Vector.Unboxed as V
import Data.Complex

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Generate inputs
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
generateInputs :: StateT r GeneratorState m (V.Vector (Double, Double))
generateInputs = get >>= \GeneratorState {tsim, chunkSize} -> V.replicateM chunkSize generateInput

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- generate input
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
generateInput :: StateT r GeneratorState m (Double, Double)
generateInput
  = StateT
      {getStateT
         = \cont generatorState@(GeneratorState {freqs, oscIDs, envs, tsim, dt, r'}) ->
             cont (polar $ V.foldl' (\s xe -> s + uncurry mkPolar xe) (0 :+ 0)
                                       (V.zip (V.accumulate (\r0 (r1, ton, toff) ->
                                                               r0 + r r' r1 ton tsim - r r' r1 toff tsim
                                                            )
                                                            (V.replicate (V.length freqs) 0)
                                                            (V.zip oscIDs envs)
                                              )
                                              (V.map (phi tsim) freqs)
                                       )
                                    )
                  generatorState {tsim = tsim + dt}
      }

----------------------------------------------------------------------------------------------------
-- amplitude of oscillator
----------------------------------------------------------------------------------------------------
r :: Double -> Double -> Double -> Double -> Double
r r' rmax t0 t
  | t <= t0   = 0
  | t <= tmax = r' * t + q
  | otherwise = rmax
  where tmax = (rmax - q) / r'
        q    = -r' * t0

----------------------------------------------------------------------------------------------------
-- phase of oscillator
----------------------------------------------------------------------------------------------------
phi :: Double -> Double -> Double
phi t freq = (- 2) * pi * (x - fromIntegral (floor x)) + pi
  where x = - (freq * t)


