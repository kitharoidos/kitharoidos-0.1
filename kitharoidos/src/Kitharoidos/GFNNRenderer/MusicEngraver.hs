{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNRenderer.MusicEngraver (
  grandStaffGrid, grandStaffColumn
) where

import Kitharoidos.GFNNRenderer.RendererPars
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import qualified Graphics.UI.GLUT as G
import qualified Data.Vector.Unboxed as V
import GHC.Float

-- | Grid of grand staves.
grandStaffGrid :: Double -> (V.Vector Double, V.Vector Double) -> RendererPars -> IO ()
grandStaffGrid staffH (vStaffOffss, hStaffOffss) rendererPars
  = do grandStaffGridLedgeLines xbeg xend ybeg yend (vStaffOffss, hStaffOffss) rendererPars
       grandStaffGridLines      xbeg xend ybeg yend (vStaffOffss, hStaffOffss) rendererPars
    where xbeg        = V.head vStaffOffss - staffH
          xend        = V.last vStaffOffss + staffH
          ybeg        = V.head hStaffOffss - staffH
          yend        = V.last hStaffOffss + staffH

-- | Column of grand staves.
grandStaffColumn :: Double -> Double -> V.Vector Double -> RendererPars -> IO ()
grandStaffColumn xbeg xend hStaffOffss rendererPars
  = do grandStaffColumnLedgeLines xbeg xend hStaffOffss rendererPars
       grandStaffColumnLines      xbeg xend hStaffOffss rendererPars

-- | Grid of grand staff lines.
grandStaffGridLines :: Double -> Double -> Double -> Double -> (V.Vector Double, V.Vector Double) ->
                       RendererPars -> IO ()
grandStaffGridLines xbeg xend ybeg yend (vStaffOffss, hStaffOffss) rendererPars
  = do grandStaffColumnLines xbeg xend hStaffOffss rendererPars
       preservingMatrix $ do rotate 270 $ Vector3 0 0 (1 :: GLdouble)
                             grandStaffColumnLines ybeg yend vStaffOffss rendererPars

-- | Column of grand staff lines.
grandStaffColumnLines :: Double -> Double -> V.Vector Double -> RendererPars -> IO ()
grandStaffColumnLines xbeg xend hStaffOffss rendererPars
  = V.mapM_ (\y -> preservingMatrix $ translate (Vector3 0 y 0) >> grandStaffLines xbeg xend rendererPars)
            hStaffOffss

-- | Grand staff lines.
grandStaffLines :: Double -> Double -> RendererPars -> IO ()
grandStaffLines xbeg xend rendererPars@RendererPars {staffLineD, clefIndent}
  = do staffLines xbeg xend rendererPars
       preservingMatrix $ do translate $ Vector3 (xbeg + clefInd) ( 2 * staffLineD) 0
                             clef "G4" rendererPars

       preservingMatrix $ do translate $ Vector3 (xend + xbeg) 0 0
                             rotate (180 :: GLdouble) (Vector3 0 0 1)
                             staffLines xbeg xend rendererPars
       preservingMatrix $ do translate $ Vector3 (xbeg + clefInd) (-2 * staffLineD) 0
                             clef "F3" rendererPars
    where clefInd = clefIndent * staffLineD

-- | Staff lines.
staffLines :: Double -> Double -> RendererPars -> IO ()
staffLines xbeg xend rendererPars@RendererPars {staffLineD}
  = mapM_ (\y -> preservingMatrix $ translate (Vector3 0 y 0) >> staffLine xbeg xend rendererPars)
          (take 5 [staffLineD, 2 * staffLineD ..])

-- | Staff line.
staffLine :: Double -> Double -> RendererPars -> IO ()
staffLine xbeg xend RendererPars {staffLineWidth, staffLineColor} = do
  lineWidth $= double2Float staffLineWidth
  color        staffLineColor
  renderPrimitive LineStrip $ do
    vertex $ Vertex3 xbeg 0 0
    vertex $ Vertex3 xend 0 0
  lineWidth $= 1

-- | Clef.
clef :: String -> RendererPars -> IO ()
clef str RendererPars {staffLineD, clefSize = (w, h), clefPadding}
  = do let (halfBoxW, halfBoxH) = (boxW / 2, boxH / 2)
       get clearColor >>= color
       preservingMatrix $ do
         translate $ Vector3 halfBoxW 0 0
         renderPrimitive Quads $ do
           vertex $ Vertex3 (-halfBoxW) (-halfBoxH) 0
           vertex $ Vertex3   halfBoxW  (-halfBoxH) 0
           vertex $ Vertex3   halfBoxW    halfBoxH  0
           vertex $ Vertex3 (-halfBoxW)   halfBoxH  0

       let halfClefH = clefH / 2
       color $ Color3 0 0 (0 :: GLdouble)
       preservingMatrix $ do
         translate $ Vector3 clefP (-halfClefH) 0
         scale (boxW / (104.76 * fromIntegral (length str))) (boxH / 119.05) 0
         G.renderString G.MonoRoman str
    where (boxW , boxH)  = (clefW + 2 * clefP, clefH + 2 * clefP)
          clefP          = clefPadding * staffLineD
          (clefW, clefH) = (w * staffLineD, h * staffLineD)

-- | Grid of grand staff ledge lines.
grandStaffGridLedgeLines :: Double -> Double -> Double -> Double -> (V.Vector Double, V.Vector Double) ->
                            RendererPars -> IO ()
grandStaffGridLedgeLines xbeg xend ybeg yend (vStaffOffss, hStaffOffss) rendererPars
  = do grandStaffColumnLedgeLines xbeg xend hStaffOffss rendererPars
       preservingMatrix $ do rotate 270 $ Vector3 0 0 (1 :: GLdouble)
                             grandStaffColumnLedgeLines ybeg yend vStaffOffss rendererPars

-- | Column of grand staff ledge lines.
grandStaffColumnLedgeLines :: Double -> Double -> V.Vector Double -> RendererPars -> IO ()
grandStaffColumnLedgeLines xbeg xend hStaffOffss rendererPars
  = V.mapM_ (\y -> preservingMatrix $ do translate $ Vector3 0 y 0
                                         grandStaffLedgeLines xbeg xend rendererPars
            )
            hStaffOffss

-- | Grand staff ledge lines.
grandStaffLedgeLines :: Double -> Double -> RendererPars -> IO ()
grandStaffLedgeLines xbeg xend rendererPars
  = do staffLedgeLines xbeg xend rendererPars
       preservingMatrix $ do translate $ Vector3 (xend + xbeg) 0 0
                             rotate (180 :: GLdouble) (Vector3 0 0 1)
                             staffLedgeLines xbeg xend rendererPars

-- | Staff ledge lines.
staffLedgeLines :: Double -> Double -> RendererPars -> IO ()
staffLedgeLines xbeg xend rendererPars@RendererPars {staffLineD, ledgeLineN}
  = mapM_ (\y -> preservingMatrix $ translate (Vector3 0 y 0) >> ledgeLine xbeg xend rendererPars)
          (0 : take ledgeLineN [6 * staffLineD, 7 * staffLineD ..])

-- | Ledge line.
ledgeLine :: Double -> Double -> RendererPars -> IO ()
ledgeLine xbeg xend RendererPars {ledgeLineWidth, ledgeLineColor} = do
  lineWidth $= double2Float ledgeLineWidth
  color        ledgeLineColor
  renderPrimitive LineStrip $ do
    vertex $ Vertex3 xbeg 0   0
    vertex $ Vertex3 xend 0   0
  lineWidth $= 1
