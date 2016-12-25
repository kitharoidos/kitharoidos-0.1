{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNRenderer.RenderGFNN (
  renderGFNN
) where

import Kitharoidos.GFNNSimulator (View (View, normAvgRZs, instFreqs, normAvgRCs))
import Kitharoidos.GFNNRenderer.RendererState
import Kitharoidos.MusicCalc
import qualified Data.Vector.Unboxed as V
import Graphics.Rendering.OpenGL
import Control.ContStuff
import Control.Monad
import GHC.Float

-- | Render GFNN.
renderGFNN :: View -> StateT r RendererState m (IO ())
renderGFNN view
  = do oscRendering  <- renderOscs view
       connRendering <- renderConns view
       return $ oscRendering >> connRendering

-- | Render neural oscillators.
renderOscs :: View -> StateT r RendererState m (IO ())
renderOscs View {normAvgRZs, instFreqs}
  = StateT
      {getStateT
         = \cont rendererState@RendererState {staffLineD, oscPoss, noteHeadWidth, noteHeadHeight} ->
             cont (renderPrimitive Quads
                     (V.mapM_ (\((x, y), f, rZ) -> do
                                 let y1    = y + freqToStaffPos staffLineD f
                                     halfW = noteHeadWidth  / 2
                                     halfH = noteHeadHeight * staffLineD / 2
                                 color  $ Color4 0 0 0 rZ
                                 vertex $ Vertex3 (x - halfW) (y1 - halfH) 0
                                 vertex $ Vertex3 (x + halfW) (y1 - halfH) 0
                                 vertex $ Vertex3 (x + halfW) (y1 + halfH) 0
                                 vertex $ Vertex3 (x - halfW) (y1 + halfH) 0
                              )
                              (V.zip3 oscPoss instFreqs normAvgRZs)
                     )
                  )
                  rendererState
      }

-- | Render Hebbian connections.
renderConns :: View -> StateT r RendererState m (IO ())
renderConns View {normAvgRCs}
  = StateT
      {getStateT
         = \cont rendererState@RendererState {staffLineD, connPoss, noteHeadWidth} ->
             cont (renderPrimitive Quads
                     (V.mapM_ (\((x, y), rC) -> do
                                 let halfW = noteHeadWidth / 2
                                 color  $ Color4 0 0 0 rC
                                 vertex $ Vertex3 (x - halfW) (y - halfW) 0
                                 vertex $ Vertex3 (x + halfW) (y - halfW) 0
                                 vertex $ Vertex3 (x + halfW) (y + halfW) 0
                                 vertex $ Vertex3 (x - halfW) (y + halfW) 0
                              )
                              (V.zip connPoss normAvgRCs)
                     )
                  )
                  rendererState
      }
