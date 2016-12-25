module Experiments (
  experiment0
) where

import Kitharoidos
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import Data.Complex
import Data.List
import Graphics.Rendering.OpenGL

-- | GFNN experiments.
experiment0
  = Pars
      { receiverPars
          = ReceiverPars
              { midiDt = 1 / 10
              }
      , architectPars
          = ArchitectPars
              { inputOscLIDs  = V.singleton 0
              , hiddOscLIDs   = V.singleton 1
              , oscLPitches   = G.fromList [ V.enumFromStepN 41 0.1 400 -- 46 0.1 280
                                           , V.enumFromStepN 41 0.1 400 -- 46 0.1 280
                                           ]

              , fixedConnLIDs = V.fromList [0, 1, 2]
              , hebbConnLIDs  = V.empty
              }
      , generatorPars
          = GeneratorPars
              { freqs      = let micr p = if (p - 1) `mod` 12 == 0 || (p - 5) `mod` 12 == 0
                                            then midiPitchToFreq (fromIntegral p - 0.5)
                                            else midiPitchToFreq (fromIntegral p)
                             in
                                 -- V.generate 128 micr
                                 V.generate 128 (midiPitchToFreq . fromIntegral)
              , buffSize   = 10
              , chunkSize  = 4410
              , r'         = 1000-- 16 -- rmax / attack duration
              , inputDt    = 1 / 44100
              , maxRX      = 1e-1 -- 1e-1 NO MEMORY WITHOUT INTERNAL CONNECTIVITY!
              }
      , simulatorPars
          = SimulatorPars
              { oscLPars   = G.fromList [ ( -0.1 -- -0.01
                                          , V.replicate 2 (-1, 0)
                                          , 0.1
                                          )
                                        , ( -0.4
                                          , V.cons (1.2, 0.01) $ V.singleton (-1, 0)
                                          , 0.78
                                          )
                                        ]
              , connLPars  = G.fromList [ (0/0, 0/0, 2 * pi * (2 / 1e-2) ^ 2)
                                        , (0/0, 0/0, 1e1) -- 1e1
                                        , (0/0, 0/0, 5e2) -- 4.5e2
                                        ]
              , connKernel = \l maxRC pitchi pitchj ->
                  ( case l of
                      0 -> 1e-2 * gauss 0 2 (abs $ pitchi - pitchj)
                      1 -> if abs (pitchi - pitchj) < 0.05 then maxRC else 0
                      2 -> conn12TET maxRC 1.55 0.05 pitchi pitchj -- 1.55 0.05
                      _ -> 0

                  , 0
                  )

              , gfnnDt    = 1 / 44100
              }
      , rendererPars
          = RendererPars
              { staffLineD      = 0.06
              , staffD          = 1
              , ledgeLineN      = 3
              , clefSize        = (1.6, 1.6)
              , clefPadding     = 0.1
              , clefIndent      = 0.2
              , staffLineWidth  = 1
              , staffLineColor  = Color3 0   0   0
              , ledgeLineWidth  = 1
              , ledgeLineColor  = Color3 0.9 0.9 0.9
              , noteHeadHeight  = 1
              }
      , canvasPars
          = CanvasPars
              { canvasSize  = Size 1280 730
              , canvasColor = Color4 1 1 1 1
              }
      }
