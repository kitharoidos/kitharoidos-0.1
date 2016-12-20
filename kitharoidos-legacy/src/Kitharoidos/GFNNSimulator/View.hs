-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNSimulator.View
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

module Kitharoidos.GFNNSimulator.View (
  View (View, normAvgRZs, instFreqs, normAvgRCs)
) where

import qualified Data.Vector.Unboxed as V

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- View of GFNN for rendering
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data View = View { normAvgRZs :: V.Vector Double
                 , instFreqs  :: V.Vector Double
                 , normAvgRCs :: V.Vector Double
                 }

instance Show View where
  show View {normAvgRZs, instFreqs, normAvgRCs}
    = "View:\n\n" ++
      "  normAvgRZs: " ++ show (V.toList normAvgRZs) ++ "\n\n" ++
      "  instFreqs: "  ++ show (V.toList instFreqs)  ++ "\n\n" ++
      "  normAvgRCs: " ++ show (V.toList normAvgRCs) ++ "\n\n\n"
