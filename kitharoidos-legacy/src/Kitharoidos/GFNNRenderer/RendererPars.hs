-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNRenderer.RendererPars
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

module Kitharoidos.GFNNRenderer.RendererPars (
  RendererPars (RendererPars, staffLineD, staffD, ledgeLineN, clefSize, clefPadding, clefIndent
                            , staffLineWidth, staffLineColor, ledgeLineWidth, ledgeLineColor
                            , noteHeadHeight
               )
) where

import Graphics.Rendering.OpenGL

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Parameters for GFNN renderer
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data RendererPars = RendererPars { staffLineD      :: !Double
                                 , staffD          :: !Double
                                 , ledgeLineN      :: !Int
                                 , clefSize        :: !(Double, Double)
                                 , clefPadding     :: !Double
                                 , clefIndent      :: !Double
                                 , staffLineWidth  :: !Double
                                 , staffLineColor  :: !(Color3 Double)
                                 , ledgeLineWidth  :: !Double
                                 , ledgeLineColor  :: !(Color3 Double)
                                 , noteHeadHeight  :: !Double
                                 } deriving Show

