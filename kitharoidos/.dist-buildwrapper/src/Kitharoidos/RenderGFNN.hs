-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.RenderGFNN
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

module Kitharoidos.RenderGFNN (
  gfnnRenderer
) where

import Data.List
import Data.Complex
import Kitharoidos.GFNN
import Euterpea.IO.MUI.UISF
import Euterpea.IO.MUI.SOE
import Euterpea.IO.MUI.Widget
import Control.Arrow
import Control.SF.AuxFunctions
import qualified Control.Category as Cat
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Graphics.Rendering.OpenGL

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Render GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
gfnnRenderer :: UISF (Maybe (Stat, View)) ()
gfnnRenderer
  = title "Rendering of GFNN" $ proc mVw -> do
      oscB   <- checkbox "render neural oscillators"  True -< ()
      connB  <- checkbox "render Hebbian connections" True -< ()
      oscCanvas  -< if oscB
                      then fmap renderOscs mVw
                      else Just $ text (0, 0) "Rendering of neural oscillators disabled."
      connCanvas -< if connB
                      then fmap renderConns mVw
                      else Just $ text (0, 0) "Rendering of Hebbian connections disabled."
    where oscCanvas  = title "Neural Oscillators"  $
                       canvas (600, 150) <<< arr (fmap $ translateGraphic (300, 75))
          connCanvas = title "Hebbian Connections" $
                       canvas (600, 450) <<< arr (fmap $ translateGraphic (300, 225))

----------------------------------------------------------------------------------------------------
-- note head
noteHead :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> Graphic
noteHead width height x y maxR r
  = withColor' (Color3 v v v) (drawRegion [[Pos ('R' : show (x0, y0, x1, y1), drawing)]])
    where v                = 1.0 - (r / maxR)
          drawing          = renderPrimitive Quads (do vertex (Vertex3 x0 y0 0)
                                                       vertex (Vertex3 x1 y0 0)
                                                       vertex (Vertex3 x1 y1 0)
                                                       vertex (Vertex3 x0 y1 0)
                                                   )
          (x0, y0, x1, y1) = (x - width / 2, y - height / 2, x + width / 2, y + height / 2)
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- render neural oscillators
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
renderOscs :: (Stat, View) -> Graphic
renderOscs (Stat {oscStaff, oscXs, maxRZs1, staffLineD}, View {rZs, phiZs'})
  = overGraphics [ overGraphics $ zipWith4 (noteHead (staffLineD / 8) staffLineD)
                                           oscXs oscYs maxRZs1 rZs1
                 , oscStaff
                 ]
    where oscYs       = map (\x -> fromRational $ toRational x :: GLdouble) . V.toList $
                        V.map (negate . freqToStaffPos staffLineD1 . (/ (2 * pi))) phiZs'
          staffLineD1 = fromRational $ toRational staffLineD :: Double
          rZs1        = map (\x -> fromRational $ toRational x :: GLdouble) $ V.toList rZs

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- render Hebbian connections
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
renderConns :: (Stat, View) -> Graphic
renderConns (Stat {connGrid, connPoss = (xs, ys), maxRCs1, staffLineD}, View {rBrCs})
  = overGraphics [ overGraphics $ zipWith4 (noteHead (staffLineD / 8) (staffLineD / 8))
                                           xs ys maxRCs1 rCs1
                 , connGrid
                 ]
    where rCs1 = map (\x -> fromRational $ toRational x :: GLdouble) $ V.toList rBrCs
