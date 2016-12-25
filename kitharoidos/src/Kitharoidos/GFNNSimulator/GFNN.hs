{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNSimulator.GFNN (
  GFNN (GFNN, inputOscIDs, hiddOscIDs, maxRZs, oscEqPars, zs, zs', avgRZs, avgPhiZs', inputs
            , fixedConnIDs, hebbConnIDs, is, js, maxRCs, connEqPars, cs, cs', avgRCs
            , dt, cnt
       ),
  emptyGFNN
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import Data.Complex

-- | GFNN (Gradient-frequency neural network).
data GFNN = GFNN { inputOscIDs  :: !(V.Vector Int)
                 , hiddOscIDs   :: !(V.Vector Int)
                 , maxRZs       :: !(V.Vector Double)
                 , oscEqPars    :: !(G.Vector (Double, Double, V.Vector (Double, Double), Double))
                 , zs           :: !(V.Vector (Double, Double))
                 , zs'          :: !(V.Vector (Double, Double))
                 , avgRZs       :: !(V.Vector Double)
                 , avgPhiZs'    :: !(V.Vector Double)
                 , inputs       :: !(V.Vector (Complex Double))

                 , fixedConnIDs :: !(V.Vector Int)
                 , hebbConnIDs  :: !(V.Vector Int)
                 , is           :: !(V.Vector Int)
                 , js           :: !(V.Vector Int)
                 , maxRCs       :: !(V.Vector Double)
                 , connEqPars   :: !(G.Vector (Double, Double, Double))
                 , cs           :: !(V.Vector (Double, Double))
                 , cs'          :: !(V.Vector (Double, Double))
                 , avgRCs       :: !(V.Vector Double)

                 , dt           :: !Double
                 , cnt          :: !Int
                 }

instance Show GFNN where
  show GFNN { inputOscIDs, hiddOscIDs, maxRZs, oscEqPars, zs, zs', avgRZs, avgPhiZs', inputs
            , fixedConnIDs, hebbConnIDs, is, js, maxRCs, connEqPars, cs, cs', avgRCs
            , dt, cnt
            }
    = "GFNN:\n\n" ++
      "  inputOscIDs: "  ++ show (V.toList inputOscIDs)      ++ "\n\n" ++
      "  hiddOscIDs: "   ++ show (V.toList hiddOscIDs)       ++ "\n\n" ++
      "  maxRZs: "       ++ show (V.toList maxRZs)           ++ "\n\n" ++
      "  oscEqPars: "    ++ show (G.map (\(omega, alpha, betasDeltas, epsilon) ->
                                           (omega, alpha, V.toList betasDeltas, epsilon)
                                        )
                                        oscEqPars
                                 )                           ++ "\n\n" ++
      "  zs: "           ++ show (V.toList zs)               ++ "\n\n" ++
      "  zs': "          ++ show (V.toList zs')              ++ "\n\n" ++
      "  avgRZs: "       ++ show (V.toList avgRZs)           ++ "\n\n" ++
      "  avgPhiZs': "    ++ show (V.toList avgPhiZs')        ++ "\n\n" ++
      "  inputs: "       ++ show (V.toList inputs)           ++ "\n\n" ++
      "  fixedConnIDs: " ++ show (V.toList fixedConnIDs)     ++ "\n\n" ++
      "  hebbConnIDs: "  ++ show (V.toList hebbConnIDs)      ++ "\n\n" ++
      "  is: "           ++ show (V.toList is)               ++ "\n\n" ++
      "  js: "           ++ show (V.toList js)               ++ "\n\n" ++
      "  maxRCs: "       ++ show (V.toList maxRCs)           ++ "\n\n" ++
      "  connEqPars: "   ++ show connEqPars                  ++ "\n\n" ++
      "  cs: "           ++ show (V.toList cs)               ++ "\n\n" ++
      "  cs': "          ++ show (V.toList cs')              ++ "\n\n" ++
      "  avgRCs: "       ++ show (V.toList avgRCs)           ++ "\n\n" ++
      "  dt: "           ++ show dt                          ++ "\n\n" ++
      "  cnt: "          ++ show cnt ++ "\n\n\n"

-- | Empty GFNN.
emptyGFNN :: GFNN
emptyGFNN
  = GFNN { inputOscIDs = V.empty, hiddOscIDs = V.empty
         , maxRZs = V.empty, oscEqPars = G.empty
         , zs = V.empty, zs' = V.empty, avgRZs = V.empty, avgPhiZs' = V.empty, inputs = V.empty

         , fixedConnIDs = V.empty, hebbConnIDs = V.empty, is = V.empty, js = V.empty
         , maxRCs = V.empty, connEqPars = G.empty
         , cs = V.empty, cs' = V.empty, avgRCs = V.empty

         , dt = 0, cnt = 0
         }
