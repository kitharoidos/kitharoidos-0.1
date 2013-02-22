-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.InputGenerator.InitGenerator
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

module Kitharoidos.InputGenerator.InitGenerator (
  initGenerator
) where

import Kitharoidos.InputGenerator.GeneratorPars
import Kitharoidos.InputGenerator.GeneratorState
import Control.ContStuff
import qualified Data.Vector.Unboxed as V
import Sound.PortMidi

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Initialize state of input generator
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
initGenerator :: GeneratorPars -> StateT r GeneratorState m ()
initGenerator generatorPars = do setFreqs generatorPars
                                 setOscIDs generatorPars
                                 setEnvs
                                 setEnvIA generatorPars
                                 setBuffPtrs
                                 setTsim
                                 setDt generatorPars
                                 setR' generatorPars
                                 setMaxRX generatorPars
                                 setChunkSize generatorPars

----------------------------------------------------------------------------------------------------
-- set frequencies of oscillators
----------------------------------------------------------------------------------------------------
setFreqs :: GeneratorPars -> StateT r GeneratorState m ()
setFreqs GeneratorPars {Kitharoidos.InputGenerator.GeneratorPars.freqs}
  = StateT
     {getStateT
        = \cont generatorState ->
            cont () (generatorState
                       {Kitharoidos.InputGenerator.GeneratorState.freqs = freqs}
                    )
     }

----------------------------------------------------------------------------------------------------
-- set IDs of oscillators
----------------------------------------------------------------------------------------------------
setOscIDs :: GeneratorPars -> StateT r GeneratorState m ()
setOscIDs GeneratorPars {buffSize}
  = StateT
      {getStateT
        = \cont generatorState@(GeneratorState {Kitharoidos.InputGenerator.GeneratorState.freqs}) ->
            cont () (generatorState
                       {oscIDs = V.concatMap (V.replicate buffSize) (V.enumFromN 0 $ V.length freqs)}
                    )
      }

----------------------------------------------------------------------------------------------------
-- set amplitude envelopes of oscillations
----------------------------------------------------------------------------------------------------
setEnvs :: StateT r GeneratorState m ()
setEnvs
  = StateT
      {getStateT
         = \cont generatorState@(GeneratorState {oscIDs}) ->
             cont () (generatorState {envs = V.replicate (V.length oscIDs) (0, 1 / 0, 1 / 0)})
      }

----------------------------------------------------------------------------------------------------
-- set IA of the envelopes (Yale format of sparse matrix)
----------------------------------------------------------------------------------------------------
setEnvIA :: GeneratorPars -> StateT r GeneratorState m ()
setEnvIA GeneratorPars {buffSize}
  = StateT
      {getStateT
         = \cont generatorState@(GeneratorState {Kitharoidos.InputGenerator.GeneratorState.freqs}) ->
             cont () (generatorState {envIA = V.enumFromStepN 0 buffSize (V.length freqs)})
      }

----------------------------------------------------------------------------------------------------
-- set pointers to buffers
----------------------------------------------------------------------------------------------------
setBuffPtrs :: StateT r GeneratorState m ()
setBuffPtrs
  = StateT
      {getStateT
         = \cont generatorState@(GeneratorState {Kitharoidos.InputGenerator.GeneratorState.freqs}) ->
             cont () (generatorState {buffPtrs = V.replicate (V.length freqs) 0})
      }

----------------------------------------------------------------------------------------------------
-- set simulated time
----------------------------------------------------------------------------------------------------
setTsim :: StateT r GeneratorState m ()
setTsim
  = StateT {getStateT = \cont generatorState -> cont () (generatorState {tsim = (-1 / 0)})}

----------------------------------------------------------------------------------------------------
-- set time step
----------------------------------------------------------------------------------------------------
setDt :: GeneratorPars -> StateT r GeneratorState m ()
setDt GeneratorPars {inputDt}
  = StateT
      {getStateT
         = \cont generatorState ->
             cont () (generatorState {dt = inputDt})
      }

----------------------------------------------------------------------------------------------------
-- set steepness of attack/release
----------------------------------------------------------------------------------------------------
setR' :: GeneratorPars -> StateT r GeneratorState m ()
setR' GeneratorPars {Kitharoidos.InputGenerator.GeneratorPars.r'}
  = StateT
      {getStateT
         = \cont generatorState ->
             cont () (generatorState {Kitharoidos.InputGenerator.GeneratorState.r'})
      }

----------------------------------------------------------------------------------------------------
-- set maximal amplitude
----------------------------------------------------------------------------------------------------
setMaxRX :: GeneratorPars -> StateT r GeneratorState m ()
setMaxRX GeneratorPars {Kitharoidos.InputGenerator.GeneratorPars.maxRX}
  = StateT
      {getStateT
         = \cont generatorState ->
             cont () (generatorState {Kitharoidos.InputGenerator.GeneratorState.maxRX})
      }

----------------------------------------------------------------------------------------------------
-- set size of chunk of inputs
----------------------------------------------------------------------------------------------------
setChunkSize :: GeneratorPars -> StateT r GeneratorState m ()
setChunkSize GeneratorPars {Kitharoidos.InputGenerator.GeneratorPars.chunkSize}
  = StateT
      {getStateT
         = \cont generatorState ->
             cont () (generatorState {Kitharoidos.InputGenerator.GeneratorState.chunkSize})
      }
