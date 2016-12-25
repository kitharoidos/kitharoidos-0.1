module Kitharoidos.InputGenerator.GeneratorPars (
  GeneratorPars (GeneratorPars, freqs, buffSize, chunkSize, r', inputDt, maxRX)
) where

import qualified Data.Vector.Unboxed as V

-- | Parameters of input generator.
data GeneratorPars = GeneratorPars { freqs      :: !(V.Vector Double)
                                   , buffSize   :: !Int
                                   , chunkSize  :: !Int
                                   , r'         :: !Double
                                   , inputDt    :: !Double
                                   , maxRX      :: !Double
                                   }
