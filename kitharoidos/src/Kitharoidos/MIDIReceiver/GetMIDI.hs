module Kitharoidos.MIDIReceiver.GetMIDI (
  getMIDI
) where

import Kitharoidos.MIDIReceiver.ReceiverError
import Control.Exception
import Sound.PortMidi

-- | Get MIDI messages.
getMIDI :: PMStream -> IO [PMEvent]
getMIDI str = readEvents str >>=
              (\eEsErr -> case eEsErr of
                 Left  es      -> return es
                 Right NoError -> return []
                 Right err     -> throwIO $ PMStreamReadError err
              )
