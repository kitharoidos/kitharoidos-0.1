module Kitharoidos.InputGenerator (
  GeneratorPars (GeneratorPars, freqs, buffSize, chunkSize, r', inputDt, maxRX),
  runInputGenerator, r, phi
) where

import Kitharoidos.InputGenerator.GeneratorPars
import Kitharoidos.InputGenerator.InitGenerator
import Kitharoidos.InputGenerator.GeneratorState
import Kitharoidos.InputGenerator.DriveGenerator
import Kitharoidos.InputGenerator.GenerateInputs
import Control.ContStuff
import Control.Monad
import Sound.PortMidi
import qualified Data.Vector.Unboxed as V
import Data.Complex

-- | Run input generator.
runInputGenerator :: GeneratorPars -> IO [PMEvent] -> (V.Vector (Double, Double) -> IO ()) -> IO ()
runInputGenerator generatorPars receiveMidi sendInputs
  = evalStateT emptyGeneratorState $ do
      initGenerator generatorPars
      forever $
        liftIO receiveMidi >>= driveGenerator >>= \tsim -> when (tsim >= 0) (generateInputs >>= (liftIO . sendInputs))
