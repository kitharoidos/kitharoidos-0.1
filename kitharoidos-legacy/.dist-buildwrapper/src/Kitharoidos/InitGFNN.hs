-----------------------------------------------------------------------------
--
-- Module      :  Kitharoidos.InitGFNN
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

{-# LANGUAGE Arrows, NamedFieldPuns, DisambiguateRecordFields #-}

module Kitharoidos.InitGFNN (
  initGFNN, defaultGFNN
) where

import Kitharoidos.Pars
import qualified Kitharoidos.GFNN as G
import qualified Data.Vector.Unboxed as V
import Euterpea.IO.MUI.SOE
import Euterpea.IO.MUI.Widget
import Euterpea.IO.MUI.UISF
import Euterpea.IO.Audio.Types
import Graphics.Rendering.OpenGL
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Initialize GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
initGFNN :: Pars -> G.GFNN
initGFNN pars = execState (do let scaff = buildScaff pars
                              setStat scaff
                              setDyn scaff
                          )
                          (G.GFNN {})

defaultGFNN :: G.GFNN
defaultGFNN = initGFNN $ Pars {lowestPitch = 48, numOf8vas = 2, temperament = 48}

----------------------------------------------------------------------------------------------------
-- grid made of horizontally and vertically oriented grand staves
grid :: ([GLdouble], [GLdouble]) ->
        (Color3 GLdouble -> GLdouble -> Graphic, Color3 GLdouble -> GLdouble -> Graphic) ->
        ((Color3 GLdouble -> GLdouble -> Graphic) -> GLdouble -> GLdouble -> Graphic) -> Graphic
grid (vStaffOffsets, hStaffOffsets) (vLine1, hLine1) staff1
  = overGraphics [ grandStaves (grandStaff vLine1 staff1) vStaffOffsets
                 , grandStaves (grandStaff hLine1 staff1) hStaffOffsets
                 ]

-- column of horizontally oriented grand staves or row of vertically oriented grand staves
grandStaves :: (GLdouble -> Graphic) -> [GLdouble] -> Graphic
grandStaves grandStaff1 offsets = overGraphics $ map (\offset -> grandStaff1 offset) offsets

-- grand staff oriented horizontally or vertically
grandStaff :: (Color3 GLdouble -> GLdouble -> Graphic) ->
              ((Color3 GLdouble -> GLdouble -> Graphic) -> GLdouble -> GLdouble -> Graphic) ->
              GLdouble -> Graphic
grandStaff line1 staff1 offset = overGraphics [trebleStaff, c1, bassStaff]
  where trebleStaff = staff1 line1 1 offset
        c1          = line1 (Color3 0.8 0.8 0.8) offset
        bassStaff   = staff1 line1 (- 1) offset

-- staff oriented horizontally or vertically
staff :: GLdouble -> GLint -> (Color3 GLdouble -> GLdouble -> Graphic) -> GLdouble -> GLdouble -> Graphic
staff staffLineD numOfLedgeLines line1 sign offset0 = overGraphics [ledgeLines, staffLines]
  where ledgeLines = overGraphics $ take (fromIntegral numOfLedgeLines) $
                     map (\offset -> line1 (Color3 0.8 0.8 0.8) (offset0 + sign * offset))
                         [6 * staffLineD, 7 * staffLineD ..]
        staffLines = overGraphics $ take 5 $
                     map (\offset -> line1 (Color3 0 0 0) (offset0 + sign * offset))
                         [staffLineD, 2 * staffLineD ..]

-- horizontal line
hLine :: GLdouble -> GLdouble -> Color3 GLdouble -> GLdouble -> Graphic
hLine x1 x2 col yOffset
  = withColor' col (Graphic $ renderPrimitive LineStrip (do vertex (Vertex3 x1 yOffset 0)
                                                            vertex (Vertex3 x2 yOffset 0)
                                                        )
                   )

-- vertical line
vLine :: GLdouble -> GLdouble -> Color3 GLdouble -> GLdouble -> Graphic
vLine y1 y2 col xOffset
  = withColor' col (Graphic $ renderPrimitive LineStrip (do vertex (Vertex3 xOffset y1 0)
                                                            vertex (Vertex3 xOffset y2 0)
                                                        )
                   )
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- build scaffolding for GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
buildScaff :: Pars -> Scaff
buildScaff pars = execState (do setOscLayerSizes pars
                                setOscLayerIDs
                                setOscLayerIDsIA
                                setFreqs pars

                                setCochOscIDs
                                setCochKernel
                                cochBounds <- setCochLBounds
                                setCochIndegs cochBounds

                                setBrOscIDs
                                brBounds <- setBrLBounds
                                setBrIndegs brBounds
                                setBrConnLayerIDs

                                setStaffD
                                setStaffLineD
                                setNumOfLedgeLines
                                setStaffHeight
                                setConnCanvasSize
                                setConnStaffOffsets
                            )
                            (Scaff {})

----------------------------------------------------------------------------------------------------
-- scaffolding for GFNN
----------------------------------------------------------------------------------------------------
data Scaff = Scaff { oscLayerSizes    :: V.Vector Int
                   , oscLayerIDs      :: V.Vector Int
                   , oscLayerIDsIA    :: V.Vector Int
                   , freqs            :: V.Vector Double

                   , cochOscIDs       :: V.Vector Int
                   , cochKernel       :: Double -> Double -> Double
                   , cochLBounds      :: V.Vector Int
                   , cochIndegs       :: V.Vector Int

                   , brOscIDs         :: V.Vector Int
                   , brLBounds        :: V.Vector Int
                   , brIndegs         :: V.Vector Int
                   , brConnLayerIDs   :: V.Vector Int

                   , staffD           :: Double
                   , staffLineD       :: Double
                   , numOfLedgeLines  :: Int
                   , staffHeight      :: Double
                   , connCanvasSize   :: (Double, Double)
                   , connStaffOffsets :: (V.Vector Double, V.Vector Double)
                   }

----------------------------------------------------------------------------------------------------
-- set sizes of layers of neural oscillators
----------------------------------------------------------------------------------------------------
setOscLayerSizes :: Pars -> State Scaff (V.Vector Int)
setOscLayerSizes (Pars {numOf8vas, temperament})
  = state $
    \scaff ->
      let cochSize      = numOf8vas * temperament
          oscLayerSizes = V.enumFromStepN cochSize (- floor (fromIntegral cochSize / 8)) 4
      in  (oscLayerSizes, scaff {oscLayerSizes})

----------------------------------------------------------------------------------------------------
-- set IDs of layers of neural oscillators
----------------------------------------------------------------------------------------------------
setOscLayerIDs :: State Scaff (V.Vector Int)
setOscLayerIDs
  = state $
    \scaff@(Scaff {oscLayerSizes}) ->
      let oscLayerIDs = V.concatMap (\oscLayerID -> V.replicate (oscLayerSizes V.! oscLayerID) oscLayerID)
                                    (V.enumFromN 0 (V.length oscLayerSizes))
      in  (oscLayerIDs, scaff {oscLayerIDs})

----------------------------------------------------------------------------------------------------
-- set IA (Yale format) of IDs of layers of neural oscillators
----------------------------------------------------------------------------------------------------
setOscLayerIDsIA :: State Scaff (V.Vector Int)
setOscLayerIDsIA
  = state $
    \scaff@(Scaff {oscLayerSizes}) ->
      let oscLayerIDsIA = V.scanl (+) 0 oscLayerSizes
      in  (oscLayerIDsIA, scaff {oscLayerIDsIA})

----------------------------------------------------------------------------------------------------
-- set natural frequencies of neural oscillators
----------------------------------------------------------------------------------------------------
setFreqs :: Pars -> State Scaff (V.Vector Double)
setFreqs (Pars {lowestPitch, temperament})
  = state $
    \scaff@(Scaff {oscLayerSizes}) ->
      let minFreq = 2 ** ((fromIntegral lowestPitch - 69) / 12) * 440
          freqs   = V.map (\oscRank -> minFreq * (2 ** (1 / fromIntegral temperament)) ^ oscRank)
                          (V.concatMap (V.enumFromN 0) oscLayerSizes :: V.Vector Int)
      in  (freqs, scaff {freqs})

----------------------------------------------------------------------------------------------------
-- set IDs of neural oscillators in cochlea
----------------------------------------------------------------------------------------------------
setCochOscIDs :: State Scaff (V.Vector Int)
setCochOscIDs
  = state $
    \scaff@(Scaff {oscLayerSizes}) ->
      let cochOscIDs = V.enumFromN 0 (V.head oscLayerSizes)
      in  (cochOscIDs, scaff {cochOscIDs})

----------------------------------------------------------------------------------------------------
-- set kernel of cochlear connectivity
----------------------------------------------------------------------------------------------------
setCochKernel :: State Scaff (Double -> Double -> Double)
setCochKernel
  = state $
    \scaff ->
      let cochKernel = \freq1 freq2 -> let semiToneD = 12 * logBase 2 (freq2 / freq1)
                                           e         = exp 1
                                       in  0.1 * e ** (- (semiToneD ^ 2) / (2 * 0.1 ^ 2))
      in  (cochKernel, scaff {cochKernel})

----------------------------------------------------------------------------------------------------
-- set lower bounds of neighborhoods of neural oscillators in cochlea
----------------------------------------------------------------------------------------------------
setCochLBounds :: State Scaff (V.Vector Int, V.Vector Int)
setCochLBounds
  = state $
    \scaff@(Scaff {oscLayerSizes, freqs, cochOscIDs, cochKernel}) ->
      let cochEnd             = (V.head oscLayerSizes) - 1
          cochKernelR         = case V.findIndex (\f -> cochKernel (V.head freqs) f <= 0.01) freqs of
                                  Nothing -> cochEnd
                                  Just r  -> r
          bs@(cochLBounds, _) = V.unzip $
                                V.map (\oscID -> ( max (oscID - cochKernelR) 0
                                                 , min (oscID + cochKernelR) cochEnd
                                                 )
                                      )
                                      cochOscIDs
      in  (bs, scaff {cochLBounds})

----------------------------------------------------------------------------------------------------
-- set number of neighbors for each neural oscillator in cochlea
----------------------------------------------------------------------------------------------------
setCochIndegs :: (V.Vector Int, V.Vector Int) -> State Scaff (V.Vector Int)
setCochIndegs (cochLBounds, cochUBounds)
  = state $
    \scaff ->
      let cochIndegs = V.zipWith (-) cochUBounds cochLBounds
      in  (cochIndegs, scaff {cochIndegs})

----------------------------------------------------------------------------------------------------
-- set IDs of neural oscillators in brain
----------------------------------------------------------------------------------------------------
setBrOscIDs :: State Scaff (V.Vector Int)
setBrOscIDs
  = state $
    \scaff@(Scaff {oscLayerIDsIA}) ->
      let cochSize = oscLayerIDsIA V.! 1
          brOscIDs = V.enumFromN cochSize (V.last oscLayerIDsIA - cochSize)
      in  (brOscIDs, scaff {brOscIDs})

----------------------------------------------------------------------------------------------------
-- set lower bounds of neighborhoods of neural oscillators in brain
----------------------------------------------------------------------------------------------------
setBrLBounds :: State Scaff (V.Vector Int, V.Vector Int)
setBrLBounds
  = state $
    \scaff@(Scaff {oscLayerIDs, oscLayerIDsIA, brOscIDs}) ->
      let bs@(brLBounds, _) = V.unzip $
                              V.map (\oscID ->
                                       let thisOscLayerID    =  oscLayerIDs   V.! oscID
                                           prevOscLayerStart =  oscLayerIDsIA V.! (thisOscLayerID - 1)
                                           thisOscLayerEnd   = (oscLayerIDsIA V.! (thisOscLayerID + 1)) - 1
                                       in  (prevOscLayerStart, thisOscLayerEnd)
                                    )
                                    brOscIDs
      in  (bs, scaff {brLBounds})

----------------------------------------------------------------------------------------------------
-- set number of neighbors for each neural oscillator in brain
----------------------------------------------------------------------------------------------------
setBrIndegs :: (V.Vector Int, V.Vector Int) -> State Scaff (V.Vector Int)
setBrIndegs (brLBounds, brUBounds)
  = state $
    \scaff ->
      let brIndegs = V.zipWith (-) brUBounds brLBounds
      in  (brIndegs, scaff {brIndegs})

----------------------------------------------------------------------------------------------------
-- set IDs of layers of Hebbian connections
----------------------------------------------------------------------------------------------------
setBrConnLayerIDs :: State Scaff (V.Vector Int)
setBrConnLayerIDs
  = state $
    \scaff@(Scaff {oscLayerSizes}) ->
      let brConnLayerIDs0 = V.enumFromN 0 (2 * (V.length oscLayerSizes - 1))
          brConnLayerIDs  = V.concatMap (\brConnLayerID ->
                                           V.replicate (cmpBrConnLayerSize oscLayerSizes brConnLayerID)
                                                       brConnLayerID
                                        )
                                        brConnLayerIDs0
      in  (brConnLayerIDs, scaff {brConnLayerIDs})
    where cmpBrConnLayerSize oscLayerSizes brConnLayerID
            = let oscLayer1ID = quot brConnLayerID 2
                  oscLayer2ID = oscLayer1ID + 1
              in  if mod brConnLayerID 2 == 0
                    then (oscLayerSizes V.! oscLayer1ID) * (oscLayerSizes V.! oscLayer2ID)
                    else (oscLayerSizes V.! oscLayer2ID) * ((oscLayerSizes V.! oscLayer2ID) - 1)

----------------------------------------------------------------------------------------------------
-- set distance between staves for rendering Hebbian connections (multiple of dist. betw. staff lines)
----------------------------------------------------------------------------------------------------
setStaffD :: State Scaff Double
setStaffD
  = state $
    \scaff ->
      let staffD = 2.5
      in  (staffD, scaff {staffD})

----------------------------------------------------------------------------------------------------
-- set distance between staff lines
----------------------------------------------------------------------------------------------------
setStaffLineD :: State Scaff Double
setStaffLineD
  = state $
    \scaff ->
      let staffLineD = 8
      in  (staffLineD, scaff {staffLineD})

----------------------------------------------------------------------------------------------------
-- set number of ledge lines
----------------------------------------------------------------------------------------------------
setNumOfLedgeLines :: State Scaff Int
setNumOfLedgeLines
  = state $
    \scaff ->
      let numOfLedgeLines = 3
      in  (numOfLedgeLines, scaff {numOfLedgeLines})

----------------------------------------------------------------------------------------------------
-- set staff height
----------------------------------------------------------------------------------------------------
setStaffHeight :: State Scaff Double
setStaffHeight
  = state $
    \scaff@(Scaff {staffLineD, numOfLedgeLines}) ->
      let staffHeight = fromIntegral (10 + 2 * numOfLedgeLines) * staffLineD
      in  (staffHeight, scaff {staffHeight})

----------------------------------------------------------------------------------------------------
-- set size of canvas for rendering Hebbian connections
----------------------------------------------------------------------------------------------------
setConnCanvasSize :: State Scaff (Double, Double)
setConnCanvasSize
  = state $
    \scaff@(Scaff {oscLayerSizes, staffD, staffLineD, staffHeight}) ->
      let numOfOscLayers = fromIntegral $ V.length oscLayerSizes
          staffWithD     = staffHeight + staffD * staffLineD
          connCanvasSize = ( (numOfOscLayers - 1) * staffWithD + staffHeight
                           , (numOfOscLayers - 2) * staffWithD + staffHeight
                           )
      in  (connCanvasSize, scaff {connCanvasSize})

----------------------------------------------------------------------------------------------------
-- set offsets of staves for rendering Hebbian connections
----------------------------------------------------------------------------------------------------
setConnStaffOffsets :: State Scaff (V.Vector Double, V.Vector Double)
setConnStaffOffsets
  = state $
    \scaff@(Scaff {oscLayerSizes, staffD, staffLineD, staffHeight, connCanvasSize = (w, h)}) ->
      let numOfOscLayers   = V.length oscLayerSizes
          d                = staffHeight + staffD * staffLineD
          connStaffOffsets = ( V.enumFromStepN (- (w  - staffHeight) / 2)    d  numOfOscLayers
                             , V.enumFromStepN (  (h  - staffHeight) / 2) (- d) (numOfOscLayers - 1)
                             )
      in  (connStaffOffsets, scaff {connStaffOffsets})

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set static part of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setStat :: Scaff -> State G.GFNN G.Stat
setStat scaff
  = state $
    \gfnn ->
      let stat = execState (do cpFreqs scaff
                               setOscEqPars scaff
                               setMaxRZs

                               setOscStaff scaff
                               setOscXs scaff
                               setMaxRZs1


                               cochConnEdges <- setCochJs scaff
                               setCochCs scaff cochConnEdges
                               setCochIA scaff


                               setBrIs scaff
                               setBrJs scaff
                               setBrIA scaff
                               setConnEqPars scaff
                               setMaxRCs

                               setConnGrid scaff
                               setConnPoss scaff
                               setMaxRCs1


                               cpStaffLineD scaff
                           )
                           (G.Stat {})
      in  (stat, gfnn {G.stat})

----------------------------------------------------------------------------------------------------
-- copy natural frequencies of neural oscillators
----------------------------------------------------------------------------------------------------
cpFreqs :: Scaff -> State G.Stat (V.Vector Double)
cpFreqs (Scaff {freqs})
  = state $
    \stat ->
      (freqs, stat {G.freqs})

----------------------------------------------------------------------------------------------------
-- set parameters of equations of neural oscillators
----------------------------------------------------------------------------------------------------
setOscEqPars :: Scaff -> State G.Stat (V.Vector (Double, Double, Double, Double, Double, Double))
setOscEqPars (Scaff {oscLayerIDs})
  = state $
    \stat ->
      let oscEqPars0 = V.fromList [ ( 0  , -1  , -1 ,  1   , 0 , 1 )
                                  , ( 0  , -1  , -1 ,  0   , 0 , 0.1 )
                                  , ( 1  , -1  , -1 ,  0   , 0 , 0.1 )
                                  , (-0.4,  1.2, -1 , -0.01, 0 , 0.75)
                                  ]
          oscEqPars  = V.backpermute oscEqPars0 oscLayerIDs
      in  (oscEqPars, stat {G.oscEqPars})

----------------------------------------------------------------------------------------------------
-- set maximal amplitudes of neural oscillators
----------------------------------------------------------------------------------------------------
setMaxRZs :: State G.Stat (V.Vector Double)
setMaxRZs
  = state $
    \stat@(G.Stat {G.oscEqPars}) ->
      let maxRZs = V.map (\(_, _, _, _, _, e) -> (1 / sqrt e) - 1e-308) oscEqPars
      in  (maxRZs, stat {G.maxRZs})

----------------------------------------------------------------------------------------------------
-- set staff for rendering neural oscillators
----------------------------------------------------------------------------------------------------
setOscStaff :: Scaff -> State G.Stat Graphic
setOscStaff (Scaff {staffLineD, numOfLedgeLines, connCanvasSize = (connCanvasWidth, _)})
  = state $
    \stat ->
      let oscCanvasWidth   = fromRational $ toRational connCanvasWidth :: GLdouble
          staffLineD1      = fromRational $ toRational staffLineD      :: GLdouble
          numOfLedgeLines1 = fromIntegral $ numOfLedgeLines            :: GLint
          oscStaff         = grandStaff (hLine (- oscCanvasWidth / 2) (oscCanvasWidth / 2))
                                        (staff staffLineD1 numOfLedgeLines1) 0
      in  (oscStaff, stat {G.oscStaff})

----------------------------------------------------------------------------------------------------
-- set horizontal positions of neural oscillators on the staff
----------------------------------------------------------------------------------------------------
setOscXs :: Scaff -> State G.Stat [GLdouble]
setOscXs (Scaff {oscLayerIDs, freqs, staffLineD, connStaffOffsets = (vStaffOffsets, _)})
  = state $
    \stat ->
      let oscXs = map (\x -> fromRational $ toRational x :: GLdouble) . V.toList $
                  V.zipWith (+) (V.backpermute vStaffOffsets oscLayerIDs)
                                (V.map (G.freqToStaffPos staffLineD) freqs)
      in  (oscXs, stat {G.oscXs})

----------------------------------------------------------------------------------------------------
-- set maximal amplitudes of neural oscillators
----------------------------------------------------------------------------------------------------
setMaxRZs1 :: State G.Stat [GLdouble]
setMaxRZs1
  = state $
    \stat@(G.Stat {G.maxRZs}) ->
      let maxRZs1 = map (\maxRZ -> fromRational $ toRational maxRZ :: GLdouble) $ V.toList maxRZs
      in  (maxRZs1, stat {G.maxRZs1})

----------------------------------------------------------------------------------------------------
-- set sources of cochlear connections
----------------------------------------------------------------------------------------------------
setCochJs :: Scaff -> State G.Stat (V.Vector Int, V.Vector Int)
setCochJs (Scaff {cochOscIDs, cochLBounds, cochIndegs})
  = state $
    \stat ->
      let cochIs = V.concatMap (\i -> V.replicate (cochIndegs V.! i) i) cochOscIDs
          cochJs = V.zipWith (\i j1 -> if j1 < i then j1 else j1 + 1)
                             cochIs
                             (V.concatMap (\i -> V.enumFromN (cochLBounds V.! i) (cochIndegs V.! i))
                                          cochOscIDs
                             )
      in  ((cochIs, cochJs), stat {G.cochJs})

----------------------------------------------------------------------------------------------------
-- set state of cochlear connections
----------------------------------------------------------------------------------------------------
setCochCs :: Scaff -> (V.Vector Int, V.Vector Int) -> State G.Stat (V.Vector (Double, Double))
setCochCs (Scaff {freqs, cochKernel}) (cochIs, cochJs)
  = state $
    \stat ->
      let cochC0Fun = \i j -> (cochKernel (freqs V.! (min i j)) (freqs V.! (max i j)), 0)
          cochCs    = V.zipWith cochC0Fun cochIs cochJs
      in  (cochCs, stat {G.cochCs})

----------------------------------------------------------------------------------------------------
-- set IA (Yale format) of cochlear connections
----------------------------------------------------------------------------------------------------
setCochIA :: Scaff -> State G.Stat (V.Vector Int)
setCochIA (Scaff {cochIndegs})
  = state $
    \stat ->
      let cochIA = V.scanl (+) 0 cochIndegs
      in  (cochIA, stat {G.cochIA})

----------------------------------------------------------------------------------------------------
-- set targets of Hebbian connections
----------------------------------------------------------------------------------------------------
setBrIs :: Scaff -> State G.Stat (V.Vector Int)
setBrIs (Scaff {brOscIDs, brIndegs})
  = state $
    \stat ->
      let d    = V.head brOscIDs
          brIs = V.concatMap (\i -> V.replicate (brIndegs V.! (i - d)) i) brOscIDs
      in  (brIs, stat {G.brIs})

----------------------------------------------------------------------------------------------------
-- set sources of Hebbian connections
----------------------------------------------------------------------------------------------------
setBrJs :: Scaff -> State G.Stat (V.Vector Int)
setBrJs (Scaff {brOscIDs, brLBounds, brIndegs})
  = state $
    \stat@(G.Stat {G.brIs}) ->
      let d    = V.head brOscIDs
          brJs = V.zipWith (\i j1 -> if j1 < i then j1 else j1 + 1)
                           brIs
                           (V.concatMap (\i -> V.enumFromN (brLBounds V.! (i - d)) (brIndegs V.! (i - d)))
                                        brOscIDs
                           )
      in  (brJs, stat {G.brJs})

----------------------------------------------------------------------------------------------------
-- set IA (Yale format) of Hebbian connections
----------------------------------------------------------------------------------------------------
setBrIA :: Scaff -> State G.Stat (V.Vector Int)
setBrIA (Scaff {brIndegs})
  = state $
    \stat ->
      let brIA = V.scanl (+) 0 brIndegs
      in  (brIA, stat {G.brIA})

----------------------------------------------------------------------------------------------------
-- set parameters of equations of Hebbian connections
----------------------------------------------------------------------------------------------------
setConnEqPars :: Scaff -> State G.Stat (V.Vector (Double, Double, Double))
setConnEqPars (Scaff {brConnLayerIDs})
  = state $
    \stat ->
      let connEqPars0 = V.fromList [ (0, 0, 0.1), (0, 0, 0.1)
                                   , (0, 0, 0.1), (0, 0, 0.1)
                                   , (0, 0, 0.1), (0, 0, 0.1)
                                   ]
          connEqPars  = V.backpermute connEqPars0 brConnLayerIDs
      in  (connEqPars, stat {G.connEqPars})

----------------------------------------------------------------------------------------------------
-- set maximal amplitudes of Hebbian connections
----------------------------------------------------------------------------------------------------
setMaxRCs :: State G.Stat (V.Vector Double)
setMaxRCs
  = state $
    \stat@(G.Stat {G.connEqPars}) ->
      let maxRCs = V.map (\(_, _, e) -> (1 / sqrt e) - 1e-308) connEqPars
      in  (maxRCs, stat {G.maxRCs})

----------------------------------------------------------------------------------------------------
-- set grid for rendering Hebbian connections
----------------------------------------------------------------------------------------------------
setConnGrid :: Scaff -> State G.Stat Graphic
setConnGrid (Scaff { staffLineD, numOfLedgeLines
                   , connCanvasSize   = (connCanvasWidth, connCanvasHeight)
                   , connStaffOffsets = (vStaffOffsets, hStaffOffsets)
                   }
            )
  = state $
    \stat ->
      let lines1            = ( vLine (- connCanvasHeight1 / 2) (connCanvasHeight1 / 2)
                              , hLine (- connCanvasWidth1  / 2) (connCanvasWidth1  / 2)
                              )
          staff1            = staff staffLineD1 numOfLedgeLines1
          connStaffOffsets1 = ( map (\x -> fromRational $ toRational x :: GLdouble) $
                                V.toList vStaffOffsets
                              , map (\x -> fromRational $ toRational x :: GLdouble) $
                                V.toList hStaffOffsets
                              )
          numOfLedgeLines1  = fromIntegral              numOfLedgeLines  :: GLint
          staffLineD1       = fromRational $ toRational staffLineD       :: GLdouble
          connCanvasHeight1 = fromRational $ toRational connCanvasHeight :: GLdouble
          connCanvasWidth1  = fromRational $ toRational connCanvasWidth  :: GLdouble
          connGrid          = grid connStaffOffsets1 lines1 staff1
      in  (connGrid, stat {G.connGrid})

----------------------------------------------------------------------------------------------------
-- set positions of Hebbian connections on the grid
----------------------------------------------------------------------------------------------------
setConnPoss :: Scaff -> State G.Stat ([GLdouble], [GLdouble])
setConnPoss (Scaff {oscLayerIDs, freqs, staffLineD, connStaffOffsets = (vStaffOffsets, hStaffOffsets)})
  = state $
    \stat@(G.Stat {G.brIs, G.brJs}) ->
      let connPoss = ( map (\x -> fromRational $ toRational x :: GLdouble) . V.toList $
                       V.zipWith (+) (V.backpermute vStaffOffsets $ V.backpermute oscLayerIDs brJs)
                                     (V.map (G.freqToStaffPos staffLineD) $ V.backpermute freqs brJs)
                     , map (\x -> fromRational $ toRational x :: GLdouble) . V.toList $
                       V.zipWith (-) (V.backpermute hStaffOffsets . V.map (subtract 1) $ V.backpermute oscLayerIDs brIs)
                                     (V.map (G.freqToStaffPos staffLineD) $ V.backpermute freqs brIs)
                     )
      in  (connPoss, stat {G.connPoss})

----------------------------------------------------------------------------------------------------
-- set maximal amplitudes of Hebbian connections
----------------------------------------------------------------------------------------------------
setMaxRCs1 :: State G.Stat [GLdouble]
setMaxRCs1
  = state $
    \stat@(G.Stat {G.maxRCs}) ->
      let maxRCs1 = map (\maxRC -> fromRational $ toRational maxRC :: GLdouble) $ V.toList maxRCs
      in  (maxRCs1, stat {G.maxRCs1})

----------------------------------------------------------------------------------------------------
-- copy distance between staff lines
----------------------------------------------------------------------------------------------------
cpStaffLineD :: Scaff -> State G.Stat Double
cpStaffLineD (Scaff {staffLineD})
  = state $
    \stat ->
      (staffLineD, stat {G.staffLineD})

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- set dynamic part of GFNN
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
setDyn :: Scaff -> State G.GFNN G.Dyn
setDyn scaff
  = state $
    \gfnn ->
      let dyn = execState (do setZs scaff
                              setZs' scaff

                              setBrCs scaff
                              setBrCs' scaff
                          )
                          (G.Dyn {})
      in  (dyn, gfnn {G.dyn})

----------------------------------------------------------------------------------------------------
-- set state of neural oscillators
----------------------------------------------------------------------------------------------------
setZs :: Scaff -> State G.Dyn (V.Vector (Double, Double))
setZs (Scaff {freqs})
  = state $
    \dyn ->
      let z0 = (1e-308, 0)
          zs = V.replicate (V.length freqs) z0
      in  (zs, dyn {G.zs})

----------------------------------------------------------------------------------------------------
-- set time derivatives of neural oscillators
----------------------------------------------------------------------------------------------------
setZs' :: Scaff -> State G.Dyn (V.Vector (Double, Double))
setZs' (Scaff {freqs})
  = state $
    \dyn ->
      let z0' = (0, 0)
          zs' = V.replicate (V.length freqs) z0'
      in  (zs', dyn {G.zs'})

----------------------------------------------------------------------------------------------------
-- set state of Hebbian connections
----------------------------------------------------------------------------------------------------
setBrCs :: Scaff -> State G.Dyn (V.Vector (Double, Double))
setBrCs (Scaff {brConnLayerIDs})
  = state $
    \dyn ->
      let brC0 = (1e-308, 0)
          brCs = V.replicate (V.length brConnLayerIDs) brC0
      in  (brCs, dyn {G.brCs})

----------------------------------------------------------------------------------------------------
-- set time derivatives of Hebbian connections
----------------------------------------------------------------------------------------------------
setBrCs' :: Scaff -> State G.Dyn (V.Vector (Double, Double))
setBrCs' (Scaff {brConnLayerIDs})
  = state $
    \dyn ->
      let brC0' = (0, 0)
          brCs' = V.replicate (V.length brConnLayerIDs) brC0'
      in  (brCs', dyn {G.brCs'})
