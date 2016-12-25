{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNArchitect.Architecture (
  Architecture (Architecture, oscLN, oscLIDs, inputOscIDs, hiddOscIDs, oscPitches
                            , connLIDs, fixedConnIDs, hebbConnIDs, is, js
               ),
  emptyArchitecture
) where

import qualified Data.Vector.Unboxed as V

-- | GFNN architecture.
data Architecture = Architecture { oscLN        :: !Int
                                 , oscLIDs      :: !(V.Vector Int)
                                 , inputOscIDs  :: !(V.Vector Int)
                                 , hiddOscIDs   :: !(V.Vector Int)
                                 , oscPitches   :: !(V.Vector Double)

                                 , connLIDs     :: !(V.Vector Int)
                                 , fixedConnIDs :: !(V.Vector Int)
                                 , hebbConnIDs  :: !(V.Vector Int)
                                 , is           :: !(V.Vector Int)
                                 , js           :: !(V.Vector Int)
                                 }

instance Show Architecture where
  show Architecture { oscLN, oscLIDs, inputOscIDs, hiddOscIDs, oscPitches, connLIDs
                    , fixedConnIDs, hebbConnIDs, is, js
                    }
    = "Architecture:\n\n" ++
      "  oscLN: "        ++ show oscLN                   ++ "\n\n" ++
      "  oscLIDs: "      ++ show (V.toList oscLIDs)      ++ "\n\n" ++
      "  inputOscIDs: "  ++ show (V.toList inputOscIDs)  ++ "\n\n" ++
      "  hiddOscIDs: "   ++ show (V.toList hiddOscIDs)   ++ "\n\n" ++
      "  oscPitches: "   ++ show (V.toList oscPitches)   ++ "\n\n" ++
      "  connLIDs: "     ++ show (V.toList connLIDs)     ++ "\n\n" ++
      "  fixedConnIDs: " ++ show (V.toList fixedConnIDs) ++ "\n\n" ++
      "  hebbConnIDs: "  ++ show (V.toList hebbConnIDs)  ++ "\n\n" ++
      "  is: "           ++ show (V.toList is)           ++ "\n\n" ++
      "  js: "           ++ show (V.toList js) ++ "\n\n\n"

-- | Empty architecture.
emptyArchitecture :: Architecture
emptyArchitecture = Architecture { oscLN        = 0
                                 , oscLIDs      = V.empty
                                 , inputOscIDs  = V.empty
                                 , hiddOscIDs   = V.empty
                                 , oscPitches   = V.empty

                                 , connLIDs     = V.empty
                                 , fixedConnIDs = V.empty
                                 , hebbConnIDs  = V.empty
                                 , is           = V.empty
                                 , js           = V.empty
                                 }
