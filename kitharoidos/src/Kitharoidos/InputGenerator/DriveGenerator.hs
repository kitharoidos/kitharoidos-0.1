-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.InputGenerator.DriveGenerator
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

module Kitharoidos.InputGenerator.DriveGenerator (
  driveGenerator
) where

import Kitharoidos.InputGenerator.GeneratorState
import Control.ContStuff
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.STRef
import Sound.PortMidi

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Drive input generator by MIDI
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
driveGenerator :: (Monad m) => [PMEvent] -> StateT r GeneratorState m Double
driveGenerator midi
  = StateT
      {getStateT
         = \cont generatorState@GeneratorState {envs, envIA, buffPtrs, tsim, maxRX} ->
             let tsim2 = runST $ do
                   envs1     <- V.unsafeThaw envs
                   buffPtrs1 <- V.unsafeThaw buffPtrs
                   tsim1     <- newSTRef tsim
                   mapM_ (\PMEvent {message = msg, timestamp} -> case decodeMsg msg of
                            PMMsg {status = 144, data1, data2 = 0} -> do
                              let k    = fromIntegral data1
                                  ix   = envIA V.! k + buffPtrs V.! k
                                  toff = fromIntegral timestamp / 1000
                              (r, ton, _) <- M.unsafeRead envs1 ix
                              M.unsafeWrite envs1 ix (r, ton, toff)
                              buffPtr     <- M.unsafeRead buffPtrs1 k
                              M.unsafeWrite buffPtrs1 k $ mod (buffPtr + 1) (envIA V.! 1)
                            PMMsg {status = 144, data1, data2} -> do
                              let k    = fromIntegral data1
                                  ix   = envIA V.! k + buffPtrs V.! k
                                  r    = maxRX * fromIntegral data2 / 127
                                  ton  = fromIntegral timestamp / 1000
                              M.unsafeWrite envs1 ix (r, ton, 1 / 0)
                              readSTRef tsim1 >>= \t -> when (t < 0) $ writeSTRef tsim1 ton
                            PMMsg {status = 176, data1 = 123} -> do
                              M.set envs1 (0, 1 / 0, 1 / 0)
                              M.set buffPtrs1 0
                              writeSTRef tsim1 (-1 / 0)
                            _  -> return ()
                         )
                         midi
                   V.unsafeFreeze envs1
                   V.unsafeFreeze buffPtrs1
                   readSTRef tsim1
             in cont tsim2 generatorState {tsim = tsim2}
      }
