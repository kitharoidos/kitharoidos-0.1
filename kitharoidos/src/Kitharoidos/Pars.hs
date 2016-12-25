module Kitharoidos.Pars (
  Pars (Pars, receiverPars, architectPars, generatorPars, simulatorPars, rendererPars, canvasPars)
) where

import Kitharoidos.MIDIReceiver
import Kitharoidos.GFNNArchitect
import Kitharoidos.InputGenerator
import Kitharoidos.GFNNSimulator
import Kitharoidos.GFNNRenderer
import Kitharoidos.GFNNCanvas

-- | Parameters of Kitharoidos.
data Pars = Pars { receiverPars  :: ReceiverPars
                 , architectPars :: ArchitectPars
                 , generatorPars :: GeneratorPars
                 , simulatorPars :: SimulatorPars
                 , rendererPars  :: RendererPars
                 , canvasPars    :: CanvasPars
                 }
