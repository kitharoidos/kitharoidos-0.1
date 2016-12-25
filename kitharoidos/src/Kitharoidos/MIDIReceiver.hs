{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.MIDIReceiver (
  ReceiverPars (ReceiverPars, midiDt),
  runMIDIReceiver
) where

import Kitharoidos.MIDIReceiver.ReceiverPars
import Kitharoidos.MIDIReceiver.InitReceiver
import Kitharoidos.MIDIReceiver.OpenMIDI
import Kitharoidos.MIDIReceiver.GetMIDI
import Kitharoidos.MIDIReceiver.CloseMIDI
import Kitharoidos.MIDIReceiver.TerminateReceiver
import Sound.PortMidi
import Control.Exception
import Control.Monad
import Control.Concurrent

-- | Run receiver of MIDI messages.
runMIDIReceiver :: ReceiverPars -> ([PMEvent] -> IO ()) -> IO ()
runMIDIReceiver ReceiverPars {midiDt} sendMIDI
  = bracket (bracketOnError initReceiver (const terminateReceiver) (const openMIDI))
            (\str -> finally (closeMIDI str) terminateReceiver)
            (\str -> forever $ getMIDI str >>= sendMIDI >> threadDelay delay)
    where delay = round $ midiDt * 1e6
