module Kitharoidos.GFNNRenderer (
  RendererPars (RendererPars, staffLineD, staffD, ledgeLineN, clefSize, clefPadding, clefIndent
                            , staffLineWidth, staffLineColor, ledgeLineWidth, ledgeLineColor
                            , noteHeadHeight
               ),
  runGFNNRenderer
) where

import Kitharoidos.GFNNArchitect (Architecture)
import Kitharoidos.GFNNRenderer.RendererPars
import Kitharoidos.GFNNRenderer.InitRenderer
import Kitharoidos.GFNNRenderer.RendererState
import Kitharoidos.GFNNRenderer.RenderGFNN
import Kitharoidos.GFNNSimulator (View)
import Control.ContStuff
import Control.Monad

-- | Run GFNN renderer.
runGFNNRenderer :: Architecture -> RendererPars -> IO View -> (IO () -> IO ()) -> IO ()
runGFNNRenderer architecture rendererPars readView sendRendering
  = evalStateT emptyRendererState $ do
      initRenderer architecture rendererPars >>= (liftIO . sendRendering)
      forever $ liftIO readView >>= renderGFNN >>= (liftIO . sendRendering)
