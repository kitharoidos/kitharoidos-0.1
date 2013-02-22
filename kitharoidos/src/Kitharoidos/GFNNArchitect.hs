-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.GFNNArchitect
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

module Kitharoidos.GFNNArchitect (
  ArchitectPars (ArchitectPars, inputOscLIDs, hiddOscLIDs, oscLPitches, fixedConnLIDs, hebbConnLIDs),
  Architecture (Architecture, oscLN, oscLIDs, inputOscIDs, hiddOscIDs, oscPitches
                            , connLIDs, fixedConnIDs, hebbConnIDs, is, js
               ),
  runGFNNArchitect
) where

import Kitharoidos.GFNNArchitect.ArchitectPars
import Kitharoidos.GFNNArchitect.DesignArchitecture
import Kitharoidos.GFNNArchitect.Architecture
import Control.ContStuff

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Run GFNN architect
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runGFNNArchitect :: ArchitectPars -> IO Architecture
runGFNNArchitect = evalStateT emptyArchitecture . designArchitecture
