-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNArchitect.DesignArchitecture
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNArchitect.DesignArchitecture (
  designArchitecture
) where

import Kitharoidos.GFNNArchitect.ArchitectPars
import Kitharoidos.GFNNArchitect.Architecture
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import Control.ContStuff
import Control.Monad.ST

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Design architecture of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
designArchitecture :: ArchitectPars -> StateT r Architecture m Architecture
designArchitecture architectPars
  = do setOscLN architectPars
       oscLSizes  <- getOscLSizes architectPars
       setOscLIDs oscLSizes
       setOscLayout architectPars oscLSizes
       setOscPitches architectPars

       connLSizes <- getConnLSizes architectPars oscLSizes
       setConnLayout architectPars connLSizes
       setEdges oscLSizes connLSizes
       setConnLIDs
       get

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set number of layers of oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setOscLN :: ArchitectPars -> StateT r Architecture m ()
setOscLN ArchitectPars {oscLPitches}
  = StateT
      {getStateT
         = \cont architecture -> cont () (architecture {oscLN = G.length oscLPitches})
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- for each layer of oscillators, get its size
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getOscLSizes :: ArchitectPars -> StateT r Architecture m (V.Vector Int)
getOscLSizes ArchitectPars {oscLPitches}
  = StateT
      {getStateT = \cont architecture -> cont (G.convert $ G.map V.length oscLPitches) architecture}

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- for each oscillator, set ID of its layer
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setOscLIDs :: V.Vector Int -> StateT r Architecture m ()
setOscLIDs oscLSizes
  = StateT
      {getStateT
         = \cont architecture ->
             cont () (architecture
                        {oscLIDs = V.concatMap (\(oscLID, oscLSize) -> V.replicate oscLSize oscLID)
                                               (V.indexed oscLSizes)
                        }
                     )
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set layout of oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setOscLayout :: ArchitectPars -> V.Vector Int -> StateT r Architecture m ()
setOscLayout ArchitectPars {inputOscLIDs, hiddOscLIDs} oscLSizes
  = StateT
      {getStateT
         = \cont architecture ->
             cont () (architecture
                        { inputOscIDs
                            = V.concatMap (uncurry V.enumFromN) $
                              V.backpermute (V.zip oscLBegs oscLSizes) inputOscLIDs
                        , hiddOscIDs
                            = V.concatMap (uncurry V.enumFromN) $
                              V.backpermute (V.zip oscLBegs oscLSizes) hiddOscLIDs
                        }
                     )
      }
    where oscLBegs = V.prescanl (+) 0 oscLSizes

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- for each oscillator, set its natural frequency
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setOscPitches :: ArchitectPars -> StateT r Architecture m ()
setOscPitches ArchitectPars {oscLPitches}
  = StateT
      {getStateT
         = \cont architecture ->
             cont () (architecture {oscPitches = V.concat . G.toList $ oscLPitches})
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- for each layer of connections, get its size
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getConnLSizes :: ArchitectPars -> V.Vector Int -> StateT r Architecture m (V.Vector Int)
getConnLSizes ArchitectPars {fixedConnLIDs, hebbConnLIDs} oscLSizes
  = StateT
      {getStateT
         = \cont architecture@(Architecture {oscLN}) ->
             cont (V.take (V.length fixedConnLIDs + V.length hebbConnLIDs) $
                   V.generate (2 * oscLN - 1)
                              (\i -> (oscLSizes1 V.! (mod i 2 * oscLN + quot i       2)) *
                                     (oscLSizes1 V.! (          oscLN + quot (i + 1) 2))
                              )
                  )
                  architecture
      }
    where oscLSizes1 = V.map (subtract 1) oscLSizes V.++ oscLSizes

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set layout of connections (1 = fixed connection)
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setConnLayout :: ArchitectPars -> V.Vector Int -> StateT r Architecture m ()
setConnLayout ArchitectPars {fixedConnLIDs, hebbConnLIDs} connLSizes
  = StateT
      {getStateT
         = \cont architecture ->
             cont () (architecture
                        { fixedConnIDs
                            = V.concatMap (uncurry V.enumFromN) $
                              V.backpermute (V.zip connLBegs connLSizes) fixedConnLIDs
                        , hebbConnIDs
                            = V.concatMap (uncurry V.enumFromN) $
                              V.backpermute (V.zip connLBegs connLSizes) hebbConnLIDs
                        }
                     )
      }
    where connLBegs = V.prescanl (+) 0 connLSizes

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- for each connection, set ID of its target and source oscillator
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setEdges :: V.Vector Int -> V.Vector Int -> StateT r Architecture m ()
setEdges oscLSizes connLSizes
  = StateT
      {getStateT
         = \cont architecture@(Architecture {oscLIDs}) ->
             let bounds   = V.map (\(prevOscLStart, prevOscLSize, thisOscLSize) ->
                                     (prevOscLStart, prevOscLSize + thisOscLSize)
                                  )
                                  (V.backpermute (V.zip3 (V.cons 0 $ V.scanl (+) 0 oscLSizes)
                                                         (V.cons 0 oscLSizes)
                                                         oscLSizes
                                                 )
                                                 oscLIDs
                                  )
                 (is, js) = V.unzip . V.take (V.sum connLSizes) . V.filter (uncurry (/=)) $
                            V.concatMap (\(i, (l, n)) -> V.zip (V.replicate n i) (V.enumFromN l n))
                                        (V.indexed bounds)
             in  cont () (architecture {is, js})
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- for each connection, set ID of its layer
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setConnLIDs :: StateT r Architecture m ()
setConnLIDs
  = StateT
      {getStateT
         = \cont architecture@(Architecture {oscLIDs, is, js}) ->
             cont () (architecture
                        {connLIDs
                           = V.zipWith (+) (V.backpermute oscLIDs is) (V.backpermute oscLIDs js)}
                     )
      }
