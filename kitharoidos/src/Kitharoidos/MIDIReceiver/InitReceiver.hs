module Kitharoidos.MIDIReceiver.InitReceiver (
  initReceiver
) where

import Kitharoidos.MIDIReceiver.ReceiverError
import Control.Exception
import Sound.PortMidi

-- | Receiver for MIDI messages.
initReceiver :: IO ()
initReceiver = initialize >>=
                 (\err -> case err of
                    NoError -> return ()
                    err     -> throwIO $ PMInitializationError err
                 )
