{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNSimulator.InitSimulator (
  initSimulator
) where

import Kitharoidos.GFNNArchitect (Architecture (Architecture, oscLN, oscLIDs, inputOscIDs, hiddOscIDs, oscPitches
                                               , connLIDs, fixedConnIDs, hebbConnIDs, is, js
                                               )
                                 )
import Kitharoidos.GFNNSimulator.SimulatorPars
import Kitharoidos.GFNNSimulator.GFNN
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as G
import Control.ContStuff
import Control.Monad.ST
import Data.Complex

-- | Initialize GFNN.
initSimulator :: Architecture -> SimulatorPars -> StateT r GFNN m ()
initSimulator architecture simulatorPars
  = do setOscLayout architecture
       setOscEqPars architecture simulatorPars
       setMaxRZs
       setZs
       setZs'
       setAvgRZs
       setAvgPhiZs'
       setInputs

       setConnLayout architecture
       setEdges architecture
       setConnEqPars architecture simulatorPars
       setMaxRCs
       setCs architecture simulatorPars
       setCs'
       setAvgRCs architecture
       setDt simulatorPars
       setCnt

-- | Set layout of oscillators (1 = oscillator with external input).
setOscLayout :: Architecture -> StateT r GFNN m ()
setOscLayout Architecture { Kitharoidos.GFNNArchitect.inputOscIDs
                          , Kitharoidos.GFNNArchitect.hiddOscIDs
                          }
  = StateT
      {getStateT
         = \cont gfnn -> cont () (gfnn { Kitharoidos.GFNNSimulator.GFNN.inputOscIDs
                                       , Kitharoidos.GFNNSimulator.GFNN.hiddOscIDs
                                       }
                                 )
      }

-- | Set parameters of equations of neural oscillators.
setOscEqPars :: Architecture -> SimulatorPars -> StateT r GFNN m ()
setOscEqPars Architecture  {oscLIDs, oscPitches} SimulatorPars {oscLPars}
  = StateT
      {getStateT
         = \cont gfnn ->
             cont () (gfnn
                        {oscEqPars
                           = G.zipWith (\oscPitch (alpha, betasDeltas, epsilon) ->
                                          (midiPitch2Freq oscPitch, alpha, betasDeltas, epsilon)
                                       )
                                       (G.convert oscPitches)
                                       (G.backpermute oscLPars (G.convert oscLIDs))
                        }
                     )
      }
    where midiPitch2Freq midiPitch = 2 ** ((midiPitch - 69) / 12) * 440

-- | Set maximal amplitudes of neural oscillators.
setMaxRZs :: StateT r GFNN m ()
setMaxRZs
  = StateT
      {getStateT
         = \cont gfnn@GFNN {oscEqPars} ->
             cont () (gfnn
                        {maxRZs
                           = G.convert $ G.map (\(_, _, _, e) -> 1 / sqrt e) oscEqPars
                        }
                     )
      }

-- | Set state of neural oscillators.
setZs :: StateT r GFNN m ()
setZs
  = StateT
      {getStateT
         = \cont gfnn@GFNN {maxRZs} ->
             cont () (gfnn {zs = V.map (\maxRZ -> (1e-3 * maxRZ, 0)) maxRZs})
      }

-- | Set time derivatives of neural oscillators.
setZs' :: StateT r GFNN m ()
setZs'
  = StateT
      {getStateT
         = \cont gfnn@GFNN {zs} ->
             cont () (gfnn {zs' = V.replicate (V.length zs) (0, 0)})
      }

-- | Set average amplitudes of neural oscillators.
setAvgRZs :: StateT r GFNN m ()
setAvgRZs
  = StateT
      {getStateT
         = \cont gfnn@GFNN {zs} ->
             cont () (gfnn {avgRZs = V.replicate (V.length zs) 0})
      }

-- | Set average time derivatives of phases of neural oscillators.
setAvgPhiZs' :: StateT r GFNN m ()
setAvgPhiZs'
  = StateT
      {getStateT
         = \cont gfnn@GFNN {zs} ->
             cont () (gfnn {avgPhiZs' = V.replicate (V.length zs) 0})
      }

-- | Set inputs to neural oscillators.
setInputs :: StateT r GFNN m ()
setInputs
  = StateT
      {getStateT
         = \cont gfnn@GFNN {zs} ->
             cont () (gfnn {inputs = V.replicate (V.length zs) (0 :+ 0)})
      }

-- | Set layout of connections (1 = fixed connection).
setConnLayout :: Architecture -> StateT r GFNN m ()
setConnLayout Architecture { Kitharoidos.GFNNArchitect.fixedConnIDs
                           , Kitharoidos.GFNNArchitect.hebbConnIDs
                           }
  = StateT
      {getStateT
         = \cont gfnn -> cont () (gfnn { Kitharoidos.GFNNSimulator.GFNN.fixedConnIDs
                                       , Kitharoidos.GFNNSimulator.GFNN.hebbConnIDs
                                       }
                                 )
      }

-- | Set targets and sources of connections.
setEdges :: Architecture -> StateT r GFNN m ()
setEdges Architecture {Kitharoidos.GFNNArchitect.is, Kitharoidos.GFNNArchitect.js}
  = StateT
      {getStateT
         = \cont gfnn ->
             cont () (gfnn {Kitharoidos.GFNNSimulator.GFNN.is, Kitharoidos.GFNNSimulator.GFNN.js})
      }

-- | Set parameters of equations of Hebbian connections.
setConnEqPars :: Architecture -> SimulatorPars -> StateT r GFNN m ()
setConnEqPars Architecture {connLIDs} SimulatorPars {connLPars}
  = StateT
      {getStateT
         = \cont gfnn ->
             cont () (gfnn {connEqPars = G.backpermute connLPars (G.convert connLIDs)})
      }

-- | Set maximal amplitudes of connections.
setMaxRCs :: StateT r GFNN m ()
setMaxRCs
  = StateT
      {getStateT
         = \cont gfnn@GFNN {connEqPars} ->
             cont () (gfnn
                        {maxRCs
                          = G.convert $ G.map (\(_, _, e) -> 1 / sqrt e) connEqPars
                        }
                     )
      }

-- | Set state of connections.
setCs :: Architecture -> SimulatorPars -> StateT r GFNN m ()
setCs Architecture { oscPitches, connLIDs
                   , Kitharoidos.GFNNArchitect.fixedConnIDs, Kitharoidos.GFNNArchitect.hebbConnIDs
                   , Kitharoidos.GFNNArchitect.is, Kitharoidos.GFNNArchitect.js
                   }
      SimulatorPars {connKernel}
  = StateT
      {getStateT
         = \cont gfnn@GFNN {maxRCs} ->
             cont () (gfnn
                        {cs = runST $ do
                           v <- M.unsafeNew $ V.length maxRCs
                           V.mapM_ (\i -> M.unsafeWrite v i $
                                          connKernel (connLIDs V.! i) (maxRCs V.! i)
                                                     (pitchis V.! i)  (pitchjs V.! i)
                                   )
                                   (fixedConnIDs V.++ hebbConnIDs)
                           V.unsafeFreeze v
                        }
                     )
      }
    where pitchis = V.backpermute oscPitches is
          pitchjs = V.backpermute oscPitches js

-- | Set time derivatives of connections.
setCs' :: StateT r GFNN m ()
setCs'
  = StateT
      {getStateT
         = \cont gfnn@GFNN {cs} ->
             cont () (gfnn {cs' = V.replicate (V.length cs) (0, 0)})
      }

-- | Set average amplitudes of connections.
setAvgRCs :: Architecture -> StateT r GFNN m ()
setAvgRCs Architecture { Kitharoidos.GFNNArchitect.fixedConnIDs
                       , Kitharoidos.GFNNArchitect.hebbConnIDs
                       }
  = StateT
      {getStateT
         = \cont gfnn@GFNN {maxRCs, cs} ->
             cont () (gfnn
                        {avgRCs = runST $ do v <- M.unsafeNew $ V.length maxRCs
                                             V.mapM_ (\i -> M.unsafeWrite v i $
                                                            fst (cs V.! i) / (maxRCs V.! i)
                                                     )
                                                     fixedConnIDs
                                             V.mapM_ (\i -> M.unsafeWrite v i 0)
                                                     hebbConnIDs
                                             V.unsafeFreeze v
                        }
                     )
      }

-- | Set time step.
setDt :: SimulatorPars -> StateT r GFNN m ()
setDt SimulatorPars {gfnnDt}
  = StateT
      {getStateT
         = \cont gfnn ->
             cont () (gfnn {dt = gfnnDt})
      }

-- | Set counter of steps since the last view.
setCnt :: StateT r GFNN m ()
setCnt
  = StateT
      {getStateT
         = \cont gfnn -> cont () (gfnn {cnt = 0})
      }
