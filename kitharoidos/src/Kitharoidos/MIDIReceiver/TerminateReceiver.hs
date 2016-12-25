module Kitharoidos.MIDIReceiver.TerminateReceiver (
  terminateReceiver
) where

import Kitharoidos.MIDIReceiver.ReceiverError
import Control.Exception
import Sound.PortMidi

-- | Terminate receiver of MIDI messages.
terminateReceiver :: IO ()
terminateReceiver = terminate >>=
                      (\err -> case err of
                         NoError -> return ()
                         err     -> throwIO $ PMTerminationError err
                      )
