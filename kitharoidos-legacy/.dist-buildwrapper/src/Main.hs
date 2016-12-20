-----------------------------------------------------------------------------
--
-- Module      :  Main
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

{-# LANGUAGE Arrows, NamedFieldPuns #-}

module Main where

import Kitharoidos.GFNN
import Kitharoidos.Pars
import Kitharoidos.EditPars
import Kitharoidos.InitGFNN
import Kitharoidos.RenderGFNN
import Kitharoidos.RunGFNN
import Kitharoidos.GenInput
import Euterpea.IO.MUI.Widget
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.SOE
import Control.Arrow
import qualified Control.CCA.Types as CCA
import qualified Control.Category as Cat
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Complex

main :: IO ()
main
  = runUIEx (1280, 720)  "Kitharoidos" . leftRight $ proc _ -> do
    gfnn <- topDown (proc _ -> do
               mPars <- parEditor      -< ()
               xe    <- inputGenerator -< ()
               gfnnEx -< (fmap initGFNN mPars, xe)
            ) -< ()
    gfnnRenderer -< gfnn
--  = putStr $ show $ initGFNN (Pars {lowestPitch = 60, numOf8vas = 1, temperament = 36})
