{-# LANGUAGE DeriveDataTypeable #-}

module Kitharoidos.GFNNCanvas.CanvasError (
  CanvasError (GLFWInitializationError, GLFWWindowOpeningError)
) where

import Data.Typeable
import qualified Control.Exception as E

-- | Errors thrown by GFNN canvas.
data CanvasError =  GLFWInitializationError | GLFWWindowOpeningError deriving (Show, Typeable)

instance E.Exception CanvasError
