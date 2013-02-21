-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNSimulator.RunGFNN
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

module Kitharoidos.GFNNSimulator.RunGFNN (
  runGFNN, oscEq, connEq
) where

import Kitharoidos.GFNNSimulator.GFNN
import Control.ContStuff
import Control.Monad
import Control.Monad.ST
import Control.Arrow
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as G
import Data.Complex

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Run GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runGFNN :: (Double, Double) -> StateT r GFNN m ()
runGFNN xe = do diffZs xe
                integrZs'

                diffCs
                integrCs'

                updAvgs

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- differentiate zs
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
diffZs :: (Double, Double) -> StateT r GFNN m ()
diffZs xe
  = StateT
      {getStateT
         = \cont gfnn@(GFNN {inputOscIDs, hiddOscIDs, oscEqPars, is, js, zs, zs', cs, maxRCs, inputs}) ->
             (runST $ do xs   <- V.unsafeThaw inputs
                         V.mapM_ (\ix -> M.unsafeWrite xs ix xec     ) inputOscIDs
                         V.mapM_ (\ix -> M.unsafeWrite xs ix $ 0 :+ 0) hiddOscIDs
                         V.mapM_ (\(i, (rC, phiC), maxRC, j) -> if rC > 1e-3 * maxRC
                                    then do
                                      xi <- M.unsafeRead xs i
                                      let (rZ, phiZ) = zs V.! j
                                      M.unsafeWrite xs i $ xi + mkPolar (rC * rZ) (phiC + phiZ)
                                    else return ()
                                 )
                                 (V.zip4 is cs maxRCs js)
                         V.unsafeFreeze xs

                         zs1' <- V.unsafeThaw zs'
                         V.mapM_ (\(ix, (z, xi)) ->
                                    M.unsafeWrite zs1' ix $ oscEq (oscEqPars G.! ix) z (polar xi)
                                 )
                                 (V.indexed $ V.zip zs inputs)
                         V.unsafeFreeze zs1'
             ) `seq`
             (cont () gfnn)
      }
    where xec = uncurry mkPolar xe

-- equations for amplitude and phase of canonical neural oscillator with an input
oscEq :: (Double, Double, V.Vector (Double, Double), Double) ->
         (Double, Double) -> (Double, Double) -> (Double, Double)
oscEq (f, alpha, betasDeltas, epsilon) (rZ, phiZ) (rX, thetaX)
 = ( (rZ * (alpha + sumHOTs (betas, epsilon) rZ) +
      ((rX * (epsilon * rX * rZ + cos (phiZ - thetaX)) - sqrt epsilon * (rX * cos phiZ + rZ * cos thetaX)) /
       ((1 + epsilon * rX ^ 2 - 2 * rX * sqrt epsilon * cos thetaX) *
        (1 + epsilon * rZ ^ 2 - 2 * rZ * sqrt epsilon * cos phiZ)
       )
      )
     ) * f
   , (2 * pi + sumHOTs (deltas, epsilon) rZ +
      ((rX * (sin (thetaX - phiZ) + sqrt epsilon * (rX * sin phiZ - rZ * sin thetaX))) /
       ((1 + epsilon * rX ^ 2 - 2 * rX * sqrt epsilon * cos thetaX) *
        (1 + epsilon * rZ ^ 2 - 2 * rZ * sqrt epsilon * cos phiZ) *
        rZ
       )
      )
     ) * f
   )
   where (betas, deltas) = V.unzip betasDeltas

-- sum higher order terms
sumHOTs :: (V.Vector Double, Double) -> Double -> Double
sumHOTs (pars, epsilon) rZ
  = V.sum $ V.map (\(k, par) -> (if k < p then id else (/ (1 - epsilon * rZ ^ 2))) $
                                (par * epsilon ^ k * rZ ^ (2 * (k + 1)))
                  )
                  (V.indexed pars)
    where p = V.length pars - 1

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- integrate zs' numerically
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
integrZs' :: StateT r GFNN m ()
integrZs'
  = StateT
      {getStateT
         = \cont gfnn@(GFNN {zs, zs', maxRZs, dt}) ->
             (runST $ do zs1 <- V.unsafeThaw zs
                         V.mapM_ (\(ix, ((rZ', phiZ'), maxRZ)) -> do
                                    (rZ, phiZ) <- M.unsafeRead zs1 ix
                                    M.unsafeWrite zs1 ix $
                                      first (min (maxRZ - 1e-308)) $
                                      polar $ mkPolar (rZ + dt * rZ') (phiZ + dt * phiZ')
                                 )
                                 (V.indexed $ V.zip zs' maxRZs)
                         V.unsafeFreeze zs1
             ) `seq`
             (cont () gfnn)
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- differentiate cs
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
diffCs :: StateT r GFNN m ()
diffCs
  = StateT
      {getStateT
         = \cont gfnn@(GFNN {hebbConnIDs, is, js, connEqPars, zs, cs, cs'}) ->
             (runST $ do cs1' <- V.unsafeThaw cs'
                         V.mapM_ (\ix -> M.unsafeWrite cs1' ix $
                                         connEq (connEqPars G.! ix)  (zs V.! (is V.! ix))
                                                (zs V.! (js V.! ix)) (cs V.! ix)
                                 )
                                 hebbConnIDs
                         V.unsafeFreeze cs1'
             ) `seq`
             (cont () gfnn)
      }

-- equations for amplitude and phase of Hebbian connection
connEq :: (Double, Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
connEq (deltaij, kij, epsilon) zi zj c
  = undefined

-- equations for amplitude and phase of Hebbian connection
--connEq1 :: (Double, Double, Double) -> (Double, Double) -> (Double, Double) ->
--           (Double, Double) -> (Double, Double)
--connEq1 (deltaij, kij, epsilon) (rZi, thetaZi) (rZj, thetaZj) (rC, phiC)
--  = ( - deltaij * rC +
--      (kij * rZi * rZj *
--       (epsilon * rZi * rZj * cos phiC -
--        sqrt epsilon * rZj * cos (phiC - thetaZi) -
--        sqrt epsilon * rZi * cos (phiC + thetaZj) +
--        cos (phiC - thetaZi + thetaZj)
--       ) /
--       ((1 + epsilon * rZi ^ 2 - 2 * sqrt epsilon * rZi * cos thetaZi) *
--        (1 + epsilon * rZj ^ 2 - 2 * sqrt epsilon * rZj * cos thetaZj)
--       )
--      )
--    , kij * rZi * rZj *
--      (epsilon * rZi * rZj * sin phiC -
--       sqrt epsilon * rZj * sin (phiC - thetaZi) -
--       sqrt epsilon * rZi * sin (phiC + thetaZj) +
--       sin (phiC - thetaZi + thetaZj)
--      ) /
--      ((1 + epsilon * rZi ^ 2 - 2 * sqrt epsilon * rZi * cos thetaZi) *
--       (1 + epsilon * rZj ^ 2 - 2 * sqrt epsilon * rZj * cos thetaZj) *
--       rC
--      )
--    )

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- integrate cs' numerically
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
integrCs' :: StateT r GFNN m ()
integrCs'
  = StateT
      {getStateT
         = \cont gfnn@(GFNN {hebbConnIDs, cs, cs', dt}) ->
             (runST $ do cs1 <- V.unsafeThaw cs
                         V.mapM_ (\ix -> do
                                    (rC, phiC) <- M.unsafeRead cs1 ix
                                    let (rC', phiC') = cs'    V.! ix
                                    M.unsafeWrite cs1 ix $
                                      polar $ mkPolar (rC + dt * rC') (phiC + dt * phiC')
                                 )
                                 hebbConnIDs
                         V.unsafeFreeze cs1
             ) `seq`
             (cont () gfnn)
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- update averages
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
updAvgs :: StateT r GFNN m ()
updAvgs
  = StateT
      {getStateT
         = \cont gfnn@(GFNN {hebbConnIDs, zs, zs', cs, avgRZs, avgPhiZs', avgRCs, cnt}) ->
             addTo avgRZs    (fst $ V.unzip zs ) (V.enumFromN 0 $ V.length zs ) `seq`
             addTo avgPhiZs' (snd $ V.unzip zs') (V.enumFromN 0 $ V.length zs') `seq`
             addTo avgRCs    (fst $ V.unzip cs ) hebbConnIDs                    `seq`
             cont () (gfnn {cnt = cnt + 1})
      }

----------------------------------------------------------------------------------------------------
-- add source vector to target vector on selected positions
----------------------------------------------------------------------------------------------------
addTo :: V.Vector Double -> V.Vector Double -> V.Vector Int -> V.Vector Double
addTo targ src ics
  = runST $ do targ1 <- V.unsafeThaw targ
               V.mapM_ (\ix -> do
                          t <- M.unsafeRead targ1 ix
                          M.unsafeWrite targ1 ix $ t + (src V.! ix)
                       )
                       ics
               V.unsafeFreeze targ1
