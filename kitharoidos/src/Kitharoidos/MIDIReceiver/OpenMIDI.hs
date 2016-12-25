module Kitharoidos.MIDIReceiver.OpenMIDI (
  openMIDI
) where

import Kitharoidos.MIDIReceiver.ReceiverError
import Control.Exception
import Sound.PortMidi

-- | Open stream of MIDI messages.
openMIDI :: IO PMStream
openMIDI = getDefaultInputDeviceID >>=
             (\mDevID  -> case mDevID of
                Just devID -> return devID
                Nothing    -> throwIO NoInputDeviceError
             ) >>=
             openInput >>=
             (\eStrErr -> case eStrErr of
                Left  str  -> return str
                Right err  -> throwIO $ PMStreamOpeningError err
             )
