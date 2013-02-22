-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNN
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

{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}

module Kitharoidos.GFNN (
  GFNN (GFNN, stat, dyn),
    freqToStaffPos,
    View (View, rZs, phiZs', rBrCs),
    view,
    Stat ( Stat, maxRZs, freqs, oscEqPars
               , oscStaff, oscXs, maxRZs1

               , cochCs, cochJs, cochIA

               , maxRCs, brIs, brJs, brIA, connEqPars
               , connGrid, connPoss, maxRCs1

               , staffLineD
         ),
    Dyn  ( Dyn, zs, zs'
              , brCs, brCs'
         )
) where

import qualified Data.Vector.Unboxed as V
import Euterpea.IO.MUI.SOE
import Graphics.Rendering.OpenGL

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Gradient Frequency Neural Network
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data GFNN = GFNN {stat :: Stat, dyn :: Dyn}

instance Show GFNN where
  show (GFNN {stat, dyn})
    = "gfnn:\n" ++
      "  stat:\n" ++
      show stat ++ "\n\n\n" ++
      "  dyn:\n" ++
      show dyn

----------------------------------------------------------------------------------------------------
-- compute vertical position on staff from frequency
freqToStaffPos :: Double -> Double -> Double
freqToStaffPos staffLineD freq = 3.5 * staffLineD * logBase 2 (freq / 261.626)

-- view dynamic part of GFNN for rendering
view :: Dyn -> View
view (Dyn {zs, zs', brCs})
  = View { rZs    = fst $ V.unzip zs
         , phiZs' = snd $ V.unzip zs'
         , rBrCs  = fst $ V.unzip brCs
         }

data View = View { rZs    :: V.Vector Double
                 , phiZs' :: V.Vector Double
                 , rBrCs  :: V.Vector Double
                 }
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- static part of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data Stat = Stat { freqs      :: V.Vector Double
                 , oscEqPars  :: V.Vector (Double, Double, Double, Double, Double, Double)
                 , maxRZs     :: V.Vector Double

                 , oscStaff   :: Graphic
                 , oscXs      :: [GLdouble]
                 , maxRZs1    :: [GLdouble]


                 , cochJs     :: V.Vector Int
                 , cochCs     :: V.Vector (Double, Double)
                 , cochIA     :: V.Vector Int


                 , brIs       :: V.Vector Int
                 , brJs       :: V.Vector Int
                 , brIA       :: V.Vector Int
                 , connEqPars :: V.Vector (Double, Double, Double)
                 , maxRCs     :: V.Vector Double

                 , connGrid   :: Graphic
                 , connPoss   :: ([GLdouble], [GLdouble])
                 , maxRCs1    :: [GLdouble]


                 , staffLineD :: GLdouble
                 }

instance Show Stat where
  show (Stat { freqs, oscEqPars, maxRZs, oscStaff, oscXs, maxRZs1, cochJs, cochCs, cochIA
             , brIs, brJs, brIA, connEqPars, maxRCs, connGrid, connPoss, maxRCs1, staffLineD
             }
       )
    = "    freqs: " ++ show (V.toList freqs) ++ "\n\n" ++
      "    oscEqPars: " ++ show (V.toList oscEqPars) ++ "\n\n" ++
      "    maxRZs: " ++ show (V.toList maxRZs) ++ "\n\n" ++
      "    oscXs: " ++ show oscXs ++ "\n\n" ++
      "    maxRZs1: " ++ show maxRZs1 ++ "\n\n" ++
      "    cochJs: " ++ show (V.toList cochJs) ++ "\n\n" ++
      "    cochCs: " ++ show (V.toList cochCs) ++ "\n\n" ++
      "    cochIA: " ++ show (V.toList cochIA) ++ "\n\n" ++
      "    brIs: " ++ show (V.toList brIs) ++ "\n\n" ++
      "    brJs: " ++ show (V.toList brJs) ++ "\n\n" ++
      "    brIA: " ++ show (V.toList brIA) ++ "\n\n" ++
      "    connEqPars: " ++ show (V.toList connEqPars) ++ "\n\n" ++
      "    maxRCs: " ++ show (V.toList maxRCs) ++ "\n\n" ++
      "    connPoss: " ++ show connPoss ++ "\n\n" ++
      "    maxRCs1: " ++ show maxRCs1 ++ "\n\n" ++
      "    staffLineD: " ++ show staffLineD

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- dynamic part of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data Dyn = Dyn { zs    :: V.Vector (Double, Double)
               , zs'   :: V.Vector (Double, Double)

               , brCs  :: V.Vector (Double, Double)
               , brCs' :: V.Vector (Double, Double)
               }

instance Show Dyn where
  show (Dyn {zs, zs', brCs, brCs'})
    = "    zs: " ++ show (V.toList zs) ++ "\n\n" ++
      "    zs': " ++ show (V.toList zs') ++ "\n\n" ++
      "    brCs: " ++ show (V.toList brCs) ++ "\n\n" ++
      "    brCs': " ++ show (V.toList brCs')
