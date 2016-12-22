-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos
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

module Kitharoidos (
  Pars (Pars, receiverPars, architectPars, generatorPars, simulatorPars, rendererPars, canvasPars),
  ReceiverPars (ReceiverPars, midiDt),
  ArchitectPars (ArchitectPars, inputOscLIDs, hiddOscLIDs, oscLPitches, fixedConnLIDs, hebbConnLIDs),
  GeneratorPars (GeneratorPars, freqs, buffSize, chunkSize, r', inputDt, maxRX),
  SimulatorPars (SimulatorPars, oscLPars, connLPars, connKernel, gfnnDt),
  RendererPars (RendererPars, staffLineD, staffD, ledgeLineN, clefSize, clefPadding, clefIndent
                            , staffLineWidth, staffLineColor, ledgeLineWidth, ledgeLineColor
                            , noteHeadHeight
               ),
  CanvasPars (CanvasPars, canvasSize, canvasColor),
  runKitharoidos, r, phi, oscEq, connEq,
  freqToStaffPos, midiPitchToStaffPos, freqToMidiPitch, midiPitchToFreq,
  conn12TET, gauss
) where

import Kitharoidos.MIDIReceiver
import Kitharoidos.InputGenerator
import Kitharoidos.GFNNArchitect
import Kitharoidos.GFNNSimulator
import Kitharoidos.GFNNRenderer
import Kitharoidos.GFNNCanvas
import Kitharoidos.Pars
import Kitharoidos.MusicCalc
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception
import Control.Monad
import Data.IORef

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Run Kitharoidos
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runKitharoidos :: Pars -> IO ()
runKitharoidos Pars { receiverPars, architectPars, generatorPars
                    , simulatorPars, rendererPars, canvasPars
                    } = do
  architecture <- runGFNNArchitect architectPars

  midi          <- newTBChanIO 10
  inputs        <- newTBChanIO 10
  view          <- newTBChanIO 10
  rendering     <- newTBChanIO 10
  renderingBuff <- newIORef  $ return ()

  forkIO $ runMIDIReceiver   receiverPars                         (atomically . writeTBChan midi)

  forkIO $ runInputGenerator generatorPars
                             (atomically $ readTBChan  midi)      (atomically . writeTBChan inputs)

  forkIO $ runGFNNSimulator  architecture simulatorPars
                             (atomically $ readTBChan  inputs)    (atomically . writeTBChan view)

  forkIO $ runGFNNRenderer   architecture rendererPars
                             (atomically $ readTBChan  view)      (atomically . writeTBChan rendering)

  runGFNNCanvas              canvasPars
                             ((atomically $ tryReadTBChan rendering) >>=
                                (\mRend -> case mRend of
                                   Nothing   -> readIORef renderingBuff
                                   Just rend -> writeIORef renderingBuff rend >> return rend
                                )
                             )
