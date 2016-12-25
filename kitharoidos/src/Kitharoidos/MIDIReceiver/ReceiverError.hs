{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

module Kitharoidos.MIDIReceiver.ReceiverError (
  ReceiverError ( PMInitializationError, NoInputDeviceError, PMStreamOpeningError, PMStreamReadError
                , PMStreamClosingError, PMTerminationError
                )
) where

import Sound.PortMidi
import Control.ContStuff
import Data.Typeable
import qualified Control.Exception as E

-- | Errors thrown by MIDI receiver.
data ReceiverError =  PMInitializationError PMError
                    | NoInputDeviceError
                    | PMStreamOpeningError PMError
                    | PMStreamReadError PMError
                    | PMStreamClosingError PMError
                    | PMTerminationError PMError
                    deriving (Show, Typeable)

instance E.Exception ReceiverError
