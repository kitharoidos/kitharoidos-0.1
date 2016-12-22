-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNRenderer.InitRenderer
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

{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Kitharoidos.GFNNRenderer.InitRenderer (
  initRenderer
) where

import Kitharoidos.GFNNArchitect (Architecture (Architecture, oscLN, oscLIDs, inputOscIDs, hiddOscIDs, oscPitches
                                               , connLIDs, fixedConnIDs, hebbConnIDs, is, js
                                               ),
                                 )
import Kitharoidos.GFNNRenderer.RendererPars
import Kitharoidos.GFNNRenderer.RendererState
import Kitharoidos.GFNNRenderer.MusicEngraver
import Kitharoidos.MusicCalc
import qualified Data.Vector.Unboxed as V
import Control.Monad
import Control.ContStuff

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Initialize GFNN Renderer
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
initRenderer :: Architecture -> RendererPars -> StateT r RendererState m (IO ())
initRenderer architecture rendererPars
  = do setStaffLineD rendererPars
       staffHeight  <- getStaffHeight rendererPars
       (xs, ys)     <- getCanvasLayout staffHeight architecture rendererPars
       oscStaffPoss <- getOscStaffPoss architecture rendererPars
       setOscPoss   oscStaffPoss (V.last xs, ys) architecture
       setConnPoss  oscStaffPoss (V.init xs, ys) architecture
       setNoteHeadWidth architecture rendererPars
       setNoteHeadHeight rendererPars
       connGrid  <- getConnGrid  staffHeight  (V.init xs, ys) rendererPars
       oscColumn <- getOscColumn staffHeight  (V.last xs, ys) rendererPars
       return $ connGrid >> oscColumn

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set distance between staff lines
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setStaffLineD :: RendererPars -> StateT r RendererState m ()
setStaffLineD RendererPars {Kitharoidos.GFNNRenderer.RendererPars.staffLineD}
  = StateT {getStateT
              = \cont rendererState ->
                  cont () (rendererState {Kitharoidos.GFNNRenderer.RendererState.staffLineD})
           }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- get staff height
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getStaffHeight :: RendererPars -> StateT r RendererState m Double
getStaffHeight RendererPars {Kitharoidos.GFNNRenderer.RendererPars.staffLineD, ledgeLineN}
  = StateT {getStateT
              = \cont rendererState ->
                  cont (fromIntegral (5 + ledgeLineN) * staffLineD) rendererState
           }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- get layout of canvas for rendering GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getCanvasLayout :: Double -> Architecture -> RendererPars -> StateT r RendererState m (V.Vector Double, V.Vector Double)
getCanvasLayout staffHeight Architecture {oscLN}
                            RendererPars {Kitharoidos.GFNNRenderer.RendererPars.staffLineD, staffD}
  = StateT {getStateT
              = \cont rendererState ->
                  cont ( V.enumFromStepN (- width  / 2 + staffHeight) grandStaffHeightWithD (oscLN + 1)
                       , V.enumFromStepN (- height / 2 + staffHeight) grandStaffHeightWithD oscLN
                       )
                       rendererState
           }
    where height                = width - grandStaffHeightWithD
          width                 = fromIntegral oscLN * grandStaffHeightWithD + grandStaffHeight
          grandStaffHeightWithD = grandStaffHeight + staffD * staffLineD
          grandStaffHeight      = 2 * staffHeight

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- get vertical positions of neural oscillators on staff
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getOscStaffPoss :: Architecture -> RendererPars -> StateT r RendererState m (V.Vector Double)
getOscStaffPoss Architecture {oscPitches} RendererPars {Kitharoidos.GFNNRenderer.RendererPars.staffLineD}
  = StateT
      {getStateT
         = \cont rendererState -> cont (V.map (midiPitchToStaffPos staffLineD) oscPitches) rendererState
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set positions of neural oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setOscPoss :: V.Vector Double -> (Double, V.Vector Double) -> Architecture -> StateT r RendererState m ()
setOscPoss oscStaffPoss (x, ys) Architecture {oscLIDs}
  = StateT
      {getStateT
         = \cont rendererState ->
             cont () (rendererState
                        {oscPoss = V.zip (V.map (+ x) oscStaffPoss) (V.backpermute ys oscLIDs)}
                     )
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set positions of connections on the grid
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setConnPoss :: V.Vector Double -> (V.Vector Double, V.Vector Double) -> Architecture -> StateT r RendererState m ()
setConnPoss oscStaffPoss (gridXs, gridYs) Architecture {oscLIDs, is, js}
  = StateT
      {getStateT
         = \cont rendererState ->
             cont () (rendererState
                        {connPoss
                           = V.zip (V.zipWith (+) (V.backpermute gridXs $ V.backpermute oscLIDs js)
                                                  (V.backpermute oscStaffPoss js)
                                   )
                                   (V.zipWith (+) (V.backpermute gridYs $ V.backpermute oscLIDs is)
                                                  (V.backpermute oscStaffPoss is)
                                   )
                        }
                     )
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set note head width
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setNoteHeadWidth :: Architecture -> RendererPars -> StateT r RendererState m ()
setNoteHeadWidth Architecture {oscPitches} RendererPars {Kitharoidos.GFNNRenderer.RendererPars.staffLineD}
  = StateT
      {getStateT
         = \cont rendererState ->
             cont () (rendererState
                        {noteHeadWidth
                           = min (fst $ V.foldl' (\(!d, !pos1) pitch ->
                                                    let pos2 = midiPitchToStaffPos staffLineD pitch
                                                    in  (min d (abs $ pos2 - pos1), pos2)
                                                 )
                                                 (1 / 0, 1 / 0)
                                                 oscPitches
                                 )
                                 staffLineD
                        }
                     )
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set note head height
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setNoteHeadHeight :: RendererPars -> StateT r RendererState m ()
setNoteHeadHeight RendererPars {Kitharoidos.GFNNRenderer.RendererPars.noteHeadHeight}
  = StateT
      {getStateT
         = \cont rendererState ->
             cont () (rendererState {Kitharoidos.GFNNRenderer.RendererState.noteHeadHeight})
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- get grid of staves for rendering connections
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getConnGrid :: Double -> (V.Vector Double, V.Vector Double) -> RendererPars -> StateT r RendererState m (IO ())
getConnGrid staffHeight (gridXs, gridYs) rendererPars
  = StateT
      {getStateT
         = \cont rendererState ->
             cont (grandStaffGrid staffHeight (gridXs, gridYs) rendererPars) rendererState
      }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- get column of staves for rendering neural oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getOscColumn :: Double -> (Double, V.Vector Double) -> RendererPars -> StateT r RendererState m (IO ())
getOscColumn staffHeight (x, ys) rendererPars
  = StateT
      {getStateT
         = \cont rendererState ->
             cont (grandStaffColumn (x - staffHeight) (x + staffHeight) ys rendererPars) rendererState
      }
