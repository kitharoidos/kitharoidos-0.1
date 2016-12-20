-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GenInput
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

{-# LANGUAGE Arrows, NamedFieldPuns #-}

module Kitharoidos.GenInput (
  inputGenerator
) where

import Euterpea.IO.MUI.Widget
import Euterpea.IO.MUI.UISF
import Euterpea.IO.Audio.Types
import Euterpea.IO.MIDI.MidiIO hiding (key, velocity)
import Control.Arrow
import qualified Control.CCA.Types as CCA
import Codec.Midi
import qualified Data.Vector.Unboxed as V
import Data.List
import Data.Complex

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Generate input for GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
inputGenerator :: UISF () (Double, Double)
inputGenerator
  = setSize (500, 100) $ proc _ -> do
      mi     <- selectInput -< ()
      mMidi  <- midiIn      -< mi
      rec st <- CCA.init State { xes       = V.replicate 128 (0, 0)
                               , envelopes = V.replicate 128 (0, 0)
                               , phiXes'   = V.generate 128 noteToPhi'
                               }
                  <<< (proc (st, mMidi) -> do
                         let mSetEnvelopes = case mMidi of
                               Nothing   -> id
                               Just midi -> setEnvelopes midi
                         returnA -< mSetEnvelopes . runEnvelopes $ runOscsIn st
                      ) -< (st, mMidi)
      returnA -< sumOscsIn st
    where noteToPhi' = (\note -> 2 * pi * 2 ** ((note - 69) / 12) * 440) . fromIntegral

----------------------------------------------------------------------------------------------------
-- time step
dt :: Double
dt = 1 / rate (undefined :: AudRate)
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- state of input generator
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data State = State { xes       :: V.Vector (Double, Double)
                   , envelopes :: V.Vector (Double, Int)
                   , phiXes'   :: V.Vector Double
                   }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- run input oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runOscsIn :: State -> State
runOscsIn state@(State {xes, envelopes, phiXes'}) = state {xes = V.imap runOscIn xes}
    where runOscIn oscID (rXe, phiXe)
            = ( case envelopes V.! oscID of
                  (rXeTarget, 0)     -> rXeTarget
                  (rXeTarget, delay) -> rXe + (rXeTarget - rXe) / fromIntegral delay
              , phiXe + dt * (phiXes' V.! oscID)
              )

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- run envelopes of input oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runEnvelopes :: State -> State
runEnvelopes state@(State {envelopes})   = state {envelopes = V.map runEnvelope envelopes}
    where runEnvelope (rXeTarget, delay) = (rXeTarget, max (delay - 1) 0)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- update envelopes of input oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setEnvelopes :: [MidiMessage] -> State -> State
setEnvelopes midiMsgs state@(State {envelopes})
  = state {envelopes = envelopes V.// foldl setEnvelope [] midiMsgs}
    where setEnvelope settings midiMsg
                = case midiMsg of
                    Std (NoteOn  {key, velocity}) -> let rXeTarget = fromIntegral velocity / 127
                                                     in  (key, (rXeTarget, delay0)) : settings
                    Std (NoteOff {key, velocity}) -> let rXeTarget = 0
                                                     in  (key, (rXeTarget, delay0)) : settings
                    _                             -> settings
          delay0 = round $ 0.01 * rate (undefined :: AudRate)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- sum outputs of input oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
sumOscsIn :: State -> (Double, Double)
sumOscsIn (State {xes}) = polar $ V.foldl' sumXes (0 :+ 0) xes
    where sumXes s xe = s + uncurry mkPolar xe
