-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.MusicCalc
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

module Kitharoidos.MusicCalc (
  freqToStaffPos, midiPitchToStaffPos, freqToMidiPitch, midiPitchToFreq,
  conn12TET, gauss
) where

import Data.Fixed
import Data.Maybe
import Statistics.Distribution
import Statistics.Distribution.Normal
import qualified Data.Vector.Unboxed as V

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Calculator for musical parameters
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
freqToStaffPos :: Double -> Double -> Double
freqToStaffPos staffLineD freq = midiPitchToStaffPos staffLineD $ freqToMidiPitch freq

midiPitchToStaffPos :: Double -> Double -> Double
midiPitchToStaffPos staffLineD midiPitch = (midiPitch + efCorr + bcCorr - 5 * 14) * staffLineD / 4
    where efCorr     = fromIntegral $ (midiPitch - 4.5) `div'` 12 + 1
          bcCorr     = fromIntegral $ (midiPitch + 0.5) `div'` 12

freqToMidiPitch :: Double -> Double
freqToMidiPitch freq = logBase (2 ** (1 / 12)) (freq / 8.1757989156)

midiPitchToFreq :: Double -> Double
midiPitchToFreq midiPitch = 8.1757989156 * 2 ** (midiPitch / 12)

conn12TET :: Double -> Double -> Double -> Double -> Double -> Double
conn12TET maxRC loc tol pitchi pitchj
  | nstMel  /= Nothing = maxRC * melInt12TETProf     V.! (fromJust nstMel)
  | nstHarm /= Nothing = maxRC * harmIntCls12TETProf V.! (fromJust nstHarm)
  | otherwise          = 0
    where nstMel  = findNst loc intji             melInts12TET
          nstHarm = findNst tol (intji `mod'` 12) harmIntClss12TET
          findNst tol x xs = let ds  = V.map (abs . subtract x) xs
                                 nst = V.minIndex ds
                             in  if ds V.! nst <= tol then Just nst else Nothing
          intji    = pitchi - pitchj
--          krumhansl = let cs  = V.fromList [6.3, 2.2, 3.4, 2.3, 4.3, 4.0, 2.5, 5.1, 2.3, 3.6, 2.2, 2.8, 6.3]
--                          max = V.maximum cs
--                      in V.map (/ max) cs
--          aarden2   = V.map ((+) 0.25 . negate) $
--                      V.fromList [0, 0.25, 0.19, 0.25, 0.05, 0.15, 0.25, 0.06, 0.25, 0.18, 0.25, 0.17, 0]
--                        :: V.Vector Double

harmIntClss12TET :: V.Vector Double
harmIntClss12TET
  = V.fromList [0   , 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9   , 10  , 11  , 12   ]

melInts12TET :: V.Vector Double
melInts12TET
  = V.fromList [0   ]

harmIntCls12TETProf :: V.Vector Double
harmIntCls12TETProf
  = V.fromList [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 ]
            -- [C   , Cis , D   , Es  , E   , F   , Fis , G   , As  , A   , Bb  , B   , C    ]

melInt12TETProf :: V.Vector Double
melInt12TETProf
  = V.fromList [1.00]
            -- [C   ]

gauss :: Double -> Double -> Double -> Double
gauss m sd = density (normalDistr m sd)
