module Kitharoidos.MIDIReceiver.CloseMIDI (
  closeMIDI
) where

import Kitharoidos.MIDIReceiver.ReceiverError
import Control.Exception
import Sound.PortMidi

-- | Close stream of MIDI messages.
closeMIDI :: PMStream -> IO ()
closeMIDI str = close str >>=
                  (\err -> case err of
                     NoError -> return ()
                     err     -> throwIO $ PMStreamClosingError err
                  )
