module Kitharoidos.MIDIReceiver.ReceiverPars (
  ReceiverPars (ReceiverPars, midiDt)
) where

-- | Parameters of receiver of MIDI messages.
data ReceiverPars = ReceiverPars {midiDt :: !Double}
