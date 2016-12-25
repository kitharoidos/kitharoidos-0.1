{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNSimulator.ViewGFNN (
  viewGFNN
) where

import Kitharoidos.GFNNSimulator.GFNN
import Kitharoidos.GFNNSimulator.View
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Control.ContStuff

-- | View GFNN for rendering.
viewGFNN :: StateT r GFNN m View
viewGFNN
  = StateT
      {getStateT
         = \cont gfnn@GFNN {fixedConnIDs, hebbConnIDs, maxRZs, maxRCs, avgRZs, avgPhiZs', avgRCs, cnt} ->
             cont  View { normAvgRZs = runST $ do
                            avgRZs1     <- V.unsafeThaw avgRZs
                            normAvgRZs1 <- M.unsafeNew $ M.length avgRZs1
                            V.mapM_ (\(ix, maxRZ) -> do
                                       rZSum <- M.unsafeRead avgRZs1 ix
                                       M.unsafeWrite normAvgRZs1 ix $ rZSum / (fromIntegral cnt * maxRZ)
                                       M.unsafeWrite avgRZs1 ix 0
                                    )
                                    (V.indexed maxRZs)
                            V.unsafeFreeze avgRZs1
                            V.unsafeFreeze normAvgRZs1

                        , instFreqs  = runST $ do
                            avgPhiZs1' <- V.unsafeThaw avgPhiZs'
                            instFreqs1 <- M.unsafeNew $ M.length avgPhiZs1'
                            V.mapM_ (\ix -> do
                                       phiZ'Sum <- M.unsafeRead avgPhiZs1' ix
                                       M.unsafeWrite instFreqs1 ix $ phiZ'Sum / (fromIntegral cnt * 2 * pi)
                                       M.unsafeWrite avgPhiZs1' ix 0
                                    )
                                    (V.enumFromN 0 $ M.length avgPhiZs1')
                            V.unsafeFreeze avgPhiZs1'
                            V.unsafeFreeze instFreqs1
                        , normAvgRCs = runST $ do
                            avgRCs1     <- V.unsafeThaw avgRCs
                            normAvgRCs1 <- M.unsafeNew $ M.length avgRCs1
                            V.mapM_ (\ix -> do
                                       normAvgRC <- M.unsafeRead avgRCs1 ix
                                       M.unsafeWrite normAvgRCs1 ix normAvgRC
                                    )
                                    fixedConnIDs
                            V.mapM_ (\ix -> do
                                       rCSum <- M.unsafeRead avgRCs1 ix
                                       M.unsafeWrite normAvgRCs1 ix $ rCSum / (fromIntegral cnt * (maxRCs V.! ix))
                                       M.unsafeWrite avgRCs1 ix 0
                                    )
                                    hebbConnIDs
                            V.unsafeFreeze avgRCs1
                            V.unsafeFreeze normAvgRCs1
                        }
                  gfnn {cnt = 0}
      }
