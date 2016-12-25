{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNRenderer.RendererState (
  RendererState (RendererState, staffLineD, oscPoss, connPoss, noteHeadWidth, noteHeadHeight),
  emptyRendererState
) where

import qualified Data.Vector.Unboxed as V

-- | State of GFNN renderer.
data RendererState = RendererState { staffLineD     :: !Double
                                   , oscPoss        :: !(V.Vector (Double, Double))
                                   , connPoss       :: !(V.Vector (Double, Double))
                                   , noteHeadWidth  :: !Double
                                   , noteHeadHeight :: !Double
                                   }

instance Show RendererState where
  show RendererState {staffLineD, oscPoss, connPoss, noteHeadWidth, noteHeadHeight}
    = "RendererState:\n\n" ++
      "  staffLineD: "     ++ show staffLineD          ++ "\n\n" ++
      "  oscPoss: "        ++ show (V.toList oscPoss)  ++ "\n\n" ++
      "  connPoss: "       ++ show (V.toList connPoss) ++ "\n\n" ++
      "  noteHeadWidth: "  ++ show noteHeadWidth       ++ "\n\n" ++
      "  noteHeadHeight: " ++ show noteHeadHeight      ++ "\n\n\n"

-- | Empty state.
emptyRendererState :: RendererState
emptyRendererState
  = RendererState { staffLineD     = 0
                  , oscPoss        = V.empty
                  , connPoss       = V.empty
                  , noteHeadWidth  = 0
                  , noteHeadHeight = 0
                  }
