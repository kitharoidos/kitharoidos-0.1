{-# LANGUAGE NamedFieldPuns #-}

module Kitharoidos.GFNNCanvas.DisplayCanvas (
  displayCanvas
) where

import Kitharoidos.GFNNCanvas.CanvasPars
import Kitharoidos.GFNNCanvas.CanvasError
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Exception
import Control.Monad
import System.Environment (getProgName)
import System.Exit

-- | Display canvas for rendering GFNN.
displayCanvas :: CanvasPars -> IO ()
displayCanvas CanvasPars {canvasSize, canvasColor} = do
  openWindow canvasSize
             [DisplayRGBBits 32 32 32, DisplayAlphaBits 32, DisplayDepthBits 32, DisplayDepthBits 32]
             Window >>=
    (\result -> unless result (throwIO GLFWWindowOpeningError))
  progName <- getProgName
  windowTitle         $= progName
  windowSizeCallback  $= \size@(Size w h) -> do
    viewport   $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    let rat = fromIntegral w / fromIntegral h
    ortho2D (- rat) rat (-1) 1
  windowCloseCallback $= exitSuccess

  blend               $= Enabled
  blendFunc           $= (SrcAlpha, OneMinusSrcAlpha)

  clearColor          $= canvasColor
