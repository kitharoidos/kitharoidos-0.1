-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.RunGFNN
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

{-# LANGUAGE Arrows, NamedFieldPuns, TypeFamilies #-}

module Kitharoidos.RunGFNN (
  gfnnEx
) where

import Kitharoidos.GFNN
import Kitharoidos.InitGFNN
import Graphics.Rendering.OpenGL
import Euterpea.IO.MUI.SOE
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.Widget
import Euterpea.IO.Audio.Types
import Euterpea.Music.Signal.SigFuns
import Control.SF.AuxFunctions
import Control.SF.MSF
import Control.Arrow
import Control.Monad.State.Strict
import qualified Control.CCA.Types as CCA
import qualified Data.Vector.Unboxed as V
import Data.Complex

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Run GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
gfnnEx :: UISF (Maybe GFNN, (Double, Double)) (Maybe (Stat, View))
gfnnEx
  = setSize (500, 100) $ title "Execution of GFNN" $ proc (mGFNN, xe) -> do
      connT <- radio ["fixed connections", "Hebbian connections"] 0 -< ()
      rec exState <- CCA.init exState0 -< case mGFNN of
                                            Just gfnn -> resetGFNN exState gfnn
                                            Nothing   -> runGFNN exState connT xe
      returnA -< out exState
    where exState0 = let GFNN {stat, dyn} = defaultGFNN
                     in  ExState {i = 1, tmpStat = stat, tmpDyn = dyn, tmpAvg = view dyn, out = Nothing}


----------------------------------------------------------------------------------------------------
-- time step
dt :: Double
dt = 1 / rate (undefined :: AudRate)
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- state of execution of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data ExState = ExState { i       :: Int
                       , tmpStat :: Stat
                       , tmpDyn  :: Dyn
                       , tmpAvg  :: View
                       , out     :: Maybe (Stat, View)
                       }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- run GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runGFNN :: ExState -> Int -> (Double, Double) -> ExState
runGFNN exState connT xe
  = execState (do runOscs xe
                  when (connT == 1) (void runConns)
                  manageAvg
              )
              exState

----------------------------------------------------------------------------------------------------
-- run neural oscillators
----------------------------------------------------------------------------------------------------
runOscs :: (Double, Double) -> State ExState (V.Vector (Double, Double), V.Vector (Double, Double))
runOscs xe
  = do zs' <- diffZs xe
       zs  <- integrZs'
       return (zs, zs')

-- integrate zs' numerically
integrZs' :: State ExState (V.Vector (Double, Double))
integrZs'
  = state $
    \exState@(ExState {tmpStat = Stat {maxRZs}, tmpDyn = d@(Dyn {zs, zs'})}) ->
      let zs1 = V.zipWith3 (\maxRZ (rZ, phiZ) (rZ', phiZ') ->
                              polar $ mkPolar (min (rZ + dt * rZ') maxRZ) (phiZ + dt * phiZ')
                           )
                           maxRZs zs zs'
      in  (zs1, exState {tmpDyn = d {zs = zs1}})

-- differentiate zs
diffZs :: (Double, Double) -> State ExState (V.Vector (Double, Double))
diffZs xe
  = state $
    \exState@(ExState { tmpStat = Stat {freqs, oscEqPars, maxRZs, cochCs, cochJs, cochIA, brJs, brIA}
                      , tmpDyn  = d@(Dyn {zs, brCs})
                      }
             ) ->
      let cXe   = uncurry mkPolar xe
          cochS = V.length cochIA - 1
          xis   = V.imap (\ix maxRZ ->
                            let (cs, js) = if ix < cochS
                                             then let i = cochIA V.! ix
                                                      n = (cochIA V.! (ix + 1)) - i
                                                  in  (V.slice i n cochCs, V.slice i n cochJs)
                                             else let ixb = ix - cochS
                                                      i = brIA V.! ixb
                                                      n = (brIA V.! (ixb + 1)) - i
                                                  in  (V.slice i n brCs, V.slice i n brJs)
                            in first (min maxRZ) . polar . (if ix < cochS then (+ cXe) else id) . V.foldl1 (+) .
                               V.zipWith (\(rC, phiC) (rZ, phiZ) -> mkPolar (rC * rZ) (phiC + phiZ)) cs $
                               V.backpermute zs js
                         )
                         maxRZs
          zs'   = V.zipWith4 oscEq oscEqPars freqs xis zs
      in  (zs', exState {tmpDyn = d {zs'}})



-- equations for amplitude and phase of canonical neural oscillator with an input
oscEq :: (Double, Double, Double, Double, Double, Double) -> Double ->
         (Double, Double) -> (Double, Double) -> (Double, Double)
oscEq (alpha, beta1, beta2, delta1, delta2, epsilon) f (rX, thetaX) (rZ, phiZ)
 = ( f * (rZ * (alpha + beta1 * rZ ^ 2 + (beta2 * epsilon * rZ ^ 4 / (1 - epsilon * rZ ^ 2))) +
          ((rX * (epsilon * rX * rZ + cos (phiZ - thetaX)) - sqrt epsilon * (rX * cos phiZ + rZ * cos thetaX)) /
           ((1 + epsilon * rX ^ 2 - 2 * rX * sqrt epsilon * cos thetaX) *
            (1 + epsilon * rZ ^ 2 - 2 * rZ * sqrt epsilon * cos phiZ)
           )
          )
         )
   , 2 * pi * f + delta1 * rZ ^ 2 + (delta2 * epsilon * rZ ^ 4 / (1 - epsilon * rZ ^ 2)) +
     ((rX * (sin (thetaX - phiZ) + sqrt epsilon * (rX * sin phiZ - rZ * sin thetaX))) /
      ((1 + epsilon * rX ^ 2 - 2 * rX * sqrt epsilon * cos thetaX) *
       (1 + epsilon * rZ ^ 2 - 2 * rZ * sqrt epsilon * cos phiZ) *
       rZ
      )
     )
   )

----------------------------------------------------------------------------------------------------
-- run Hebbian connections
----------------------------------------------------------------------------------------------------
runConns :: State ExState (V.Vector (Double, Double), V.Vector (Double, Double))
runConns
  = do brCs' <- diffBrCs
       brCs  <- integrBrCs'
       return (brCs, brCs')

-- integrate brCs' numerically
integrBrCs' :: State ExState (V.Vector (Double, Double))
integrBrCs'
  = state $
    \exState@(ExState {tmpStat = Stat {maxRCs}, tmpDyn = d@(Dyn {brCs, brCs'})}) ->
      let brCs1 = V.zipWith3 (\maxRC (rC, phiC) (rC', phiC') ->
                                polar $ mkPolar (min (rC + dt * rC') maxRC) (phiC + dt * phiC')
                             )
                             maxRCs brCs brCs'
      in  (brCs1, exState {tmpDyn = d {brCs = brCs1}})

-- differentiate brCs
diffBrCs :: State ExState (V.Vector (Double, Double))
diffBrCs
  = state $
    \exState@(ExState { tmpStat = Stat {brIs, brJs, connEqPars}
                      , tmpDyn  = d@(Dyn {zs, brCs})
                      }
             ) ->
      let brCs' = V.zipWith4 connEq connEqPars (V.backpermute zs brIs) (V.backpermute zs brJs) brCs
      in  (brCs', exState {tmpDyn = d {brCs'}})

-- equations for amplitude and phase of Hebbian connection
connEq :: (Double, Double, Double) -> (Double, Double) -> (Double, Double) ->
          (Double, Double) -> (Double, Double)
connEq (deltaij, kij, epsilon) (rZi, thetaZi) (rZj, thetaZj) (rC, phiC)
  = ( - deltaij * rC +
      (kij * rZi * rZj *
       (epsilon * rZi * rZj * cos phiC -
        sqrt epsilon * rZj * cos (phiC - thetaZi) -
        sqrt epsilon * rZi * cos (phiC + thetaZj) +
        cos (phiC - thetaZi + thetaZj)
       ) /
       ((1 + epsilon * rZi ^ 2 - 2 * sqrt epsilon * rZi * cos thetaZi) *
        (1 + epsilon * rZj ^ 2 - 2 * sqrt epsilon * rZj * cos thetaZj)
       )
      )
    , kij * rZi * rZj *
      (epsilon * rZi * rZj * sin phiC -
       sqrt epsilon * rZj * sin (phiC - thetaZi) -
       sqrt epsilon * rZi * sin (phiC + thetaZj) +
       sin (phiC - thetaZi + thetaZj)
      ) /
      ((1 + epsilon * rZi ^ 2 - 2 * sqrt epsilon * rZi * cos thetaZi) *
       (1 + epsilon * rZj ^ 2 - 2 * sqrt epsilon * rZj * cos thetaZj) *
       rC
      )
    )

----------------------------------------------------------------------------------------------------
-- manage average
----------------------------------------------------------------------------------------------------
manageAvg :: State ExState (Int, View, Maybe (Stat, View))
manageAvg
  = state $
    \exState@(ExState {i, tmpStat, tmpDyn, tmpAvg, out}) ->
      let log@(j, tmpAvgj, outj) = case i of
                                     100 -> (1, view tmpDyn, Just (tmpStat, tmpAvg))
                                     i   -> (i + 1, avgVws (i + 1) tmpAvg (view tmpDyn), Nothing)
      in  (log, exState {i = j, tmpAvg = tmpAvgj, out = outj})

avgVws :: Int -> View -> View -> View
avgVws j (View {rZs = rZsi, phiZs' = phiZsi', rBrCs = rBrCsi})
         (View {rZs = rZsj, phiZs' = phiZsj', rBrCs = rBrCsj})
  = View { rZs = avgVs j1 rZsi rZsj
         , phiZs' = avgVs j1 phiZsi' phiZsj'
         , rBrCs = avgVs j1 rBrCsi rBrCsj
         }
    where j1 = fromIntegral j

avgVs :: Double -> V.Vector Double -> V.Vector Double -> V.Vector Double
avgVs j = V.zipWith (\ xi xj -> xi + (xj - xi) / j)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- reset GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
resetGFNN :: ExState -> GFNN -> ExState
resetGFNN (ExState {tmpStat, tmpAvg}) (GFNN {stat, dyn})
  = ExState {i = 1, tmpStat = stat, tmpDyn = dyn, tmpAvg = view dyn, out = Just (tmpStat, tmpAvg)}
