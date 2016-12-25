{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.InputGenerator.GeneratorState (
  GeneratorState (GeneratorState, freqs, oscIDs, envs, envIA, buffPtrs, tsim, dt, r', maxRX, chunkSize),
  emptyGeneratorState
) where

import qualified Data.Vector.Unboxed as V

-- | State of input generator.
data GeneratorState = GeneratorState { freqs     :: !(V.Vector Double)
                                     , oscIDs    :: !(V.Vector Int)
                                     , envs      :: !(V.Vector (Double, Double, Double))
                                     , envIA     :: !(V.Vector Int)
                                     , buffPtrs  :: !(V.Vector Int)
                                     , tsim      :: !Double
                                     , dt        :: !Double
                                     , r'        :: !Double
                                     , maxRX     :: !Double
                                     , chunkSize :: !Int
                                     }

instance Show GeneratorState where
  show GeneratorState {freqs, oscIDs, envs, envIA, buffPtrs, tsim, dt, r', maxRX, chunkSize}
    = "generatorState: "   ++ "\n\n" ++
      "  freqs: "    ++ show (V.toList freqs)    ++ "\n\n" ++
      "  oscIDs: "   ++ show (V.toList oscIDs)   ++ "\n\n" ++
      "  envs: "     ++ show (V.toList envs)     ++ "\n\n" ++
      "  envIA: "    ++ show (V.toList envIA)    ++ "\n\n" ++
      "  buffPtrs: " ++ show (V.toList buffPtrs) ++ "\n\n" ++
      "  tsim: "     ++ show tsim                ++ "\n\n" ++
      "  dt: "       ++ show dt                  ++ "\n\n" ++
      "  r': "       ++ show r'                  ++ "\n\n" ++
      "  maxRX: "    ++ show maxRX               ++ "\n\n" ++
      "  chunkSize:" ++ show chunkSize ++ "\n\n\n"

-- | Empty state.
emptyGeneratorState :: GeneratorState
emptyGeneratorState = GeneratorState { freqs     = V.empty
                                     , oscIDs    = V.empty
                                     , envs      = V.empty
                                     , envIA     = V.empty
                                     , buffPtrs  = V.empty
                                     , tsim      = 0 / 0
                                     , dt        = 0 / 0
                                     , r'        = 0 / 0
                                     , maxRX     = 0 / 0
                                     , chunkSize = 0
                                     }
