-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNArchitect.ArchitectPars
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

module Kitharoidos.GFNNArchitect.ArchitectPars (
  ArchitectPars (ArchitectPars, inputOscLIDs, hiddOscLIDs, oscLPitches, fixedConnLIDs, hebbConnLIDs)
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Parameters for GFNN architect
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data ArchitectPars = ArchitectPars { inputOscLIDs  :: !(V.Vector Int)
                                   , hiddOscLIDs   :: !(V.Vector Int)
                                   , oscLPitches   :: !(G.Vector (V.Vector Double))
                                   , fixedConnLIDs :: !(V.Vector Int)
                                   , hebbConnLIDs  :: !(V.Vector Int)
                                   }

instance Show ArchitectPars where
  show ArchitectPars {inputOscLIDs, hiddOscLIDs, oscLPitches, fixedConnLIDs, hebbConnLIDs}
    = "ArchitectPars:" ++ "\n\n" ++
      "  inputOscLIDs"  ++ show (V.toList inputOscLIDs)      ++ "\n\n" ++
      "  hiddOscLIDs"   ++ show (V.toList hiddOscLIDs)       ++ "\n\n" ++
      "  oscLPitches:"  ++ show (G.map V.toList oscLPitches) ++ "\n\n" ++
      "  fixedConnLIDs" ++ show (V.toList fixedConnLIDs)     ++ "\n\n" ++
      "  hebbConnLIDs"  ++ show (V.toList hebbConnLIDs)      ++ "\n\n\n"

