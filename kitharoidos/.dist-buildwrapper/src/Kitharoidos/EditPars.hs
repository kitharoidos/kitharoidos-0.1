-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.EditPars
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

module Kitharoidos.EditPars (
  parEditor
) where

import Kitharoidos.Pars
import Euterpea.Music.Note.Music
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.Widget
import Control.Arrow
import Control.SF.AuxFunctions

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Edit parameters of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
parEditor :: UISF () (Maybe Pars)
parEditor
  = setSize (500, 270) . title "Parameters of GFNN" $ proc _ -> do
      l  <- (title "natural frequency of the slowest neural oscillator (pitch)" $
             proc _ -> do
               l <- hiSlider 1 (0, 127) 60 -< ()
               display -< show $ pitch l
               returnA -< l
            ) -< ()
      n  <- (title "range of natural frequencies of neural oscillators (8vas)"  $
             proc _ -> do
               n <- hiSlider 1 (1, 10) 1 -< ()
               display -< show n
               returnA -< n
            ) -< ()
      t  <- (title "equal temperament (steps per 8va)" $
             proc _ -> do
               t <- hiSlider 1 (12, 240) 120 -< ()
               display -< show t
               returnA -< t
            ) -< ()
      ok <- edge <<< button "OK" -< ()
      returnA -< if ok
                   then Just Pars {lowestPitch = l, numOf8vas = n, temperament = t}
                   else Nothing
