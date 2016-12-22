{-#LANGUAGE NamedFieldPuns#-}

module Main (
    -- * Simulate a network of neural oscillators.
      simulateGFNN
    -- * Test input generator.
    , testInputGenerator
    -- * Test GFNN simulator.
    , testGFNNSimulator
    ) where

import Kitharoidos
import Experiments

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Complex
import Data.Colour.Names
import Data.Colour
import qualified Data.Vector.Unboxed as V
import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = undefined

-- | Simulate a network of neural oscillators listening to MIDI music
simulateGFNN :: IO ()
simulateGFNN = runKitharoidos experiment0

-- | test generator of input oscillations
testInputGenerator :: IO ()
testInputGenerator = do toFile def ("envelope" ++ ".svg") plotEnvelope
                        toFile def ("phase"    ++ ".svg") plotPhase

-- | Plot amplitude envelope of a 5s oscillation.
plotEnvelope
  = do layout_x_axis . laxis_title .= "time (s)"
       layout_y_axis . laxis_title .= "amplitude"
       plot . liftEC $ do
           plot_lines_style  . line_color .= opaque green
           plot_lines_values .= [(0,  1.5) : zip ts (map (r 1 1 1) ts)]
       plot . liftEC $ do
           plot_lines_style  . line_color .= opaque red
           plot_lines_values .= [(0, -1.5) : zip ts (map ((* (-1)) . r 1 1 3) ts)]
  where ts = [0, (1 / 44100) .. (5 - 1 / 44100)]

-- | Plot phase of a 5s oscillation.
plotPhase
  = do layout_x_axis . laxis_title .= "time (s)"
       layout_y_axis . laxis_title .= "phase (rad)"
       plot . liftEC $ do
           plot_lines_style  . line_color .= opaque cadetblue
           plot_lines_values ^= [zip ts (map (`phi` 1) ts)]
  where ts = [0, (1 / 44100) .. (5 - 1 / 44100)]

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Make preliminary experiments
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
--main = do simulate "critical" pars1 [0, 1 / 44100 .. (6 - 1 / 44100)]
--          simulate "active"   pars2 [0, 1 / 44100 .. (6 - 1 / 44100)]
--          simulate "bautin"   pars3 [0, 1 / 44100 .. (6 - 1 / 44100)]

simulate :: String -> Main.Pars -> [Double] -> IO ()
simulate fileName Main.Pars {simPars, plotPars} ts
  = plotSimResults fileName plotPars . getSimResults $ runSim simPars ts

freq :: Double
freq = 261.6255653006
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
data Pars = Pars { simPars  :: (InputOscSimPars , NeurOscSimPars)
                 , plotPars :: (InputOscPlotPars, NeurOscPlotPars)
                 }

pars1 :: Main.Pars
pars1 = Main.Pars {simPars = (rectangle, critical)    , plotPars = (inputOscPlotPars, neurOscPlotPars)}

pars2 :: Main.Pars
pars2 = Main.Pars {simPars = (rectangle, active)      , plotPars = (inputOscPlotPars, neurOscPlotPars)}

pars3 :: Main.Pars
pars3 = Main.Pars {simPars = (rectangle, bautin)      , plotPars = (inputOscPlotPars, neurOscPlotPars)}

----------------------------------------------------------------------------------------------------
-- input oscillator parameters
----------------------------------------------------------------------------------------------------
-- simulation parameters
type InputOscSimPars = (Double, Double, Double, Double, Double, Double)

rectangle :: InputOscSimPars
rectangle = (r', r', rmax, 2, 4, freq)
    where r'   = rmax / 1e-3
          rmax = 1

-- plot parameters
type InputOscPlotPars = (AlphaColour Double, AlphaColour Double)

inputOscPlotPars :: InputOscPlotPars
inputOscPlotPars = (opaque cadetblue, opaque cadetblue)


----------------------------------------------------------------------------------------------------
-- neural oscillator
----------------------------------------------------------------------------------------------------
-- simulation parameters
type NeurOscSimPars = (Double, Double, V.Vector (Double, Double), Double)

critical :: NeurOscSimPars
critical = ( freq
           , 0
           , V.replicate 2 (-1, 0)
           , 0.1
           )

active :: NeurOscSimPars
active = ( freq
         , 0.1
         , V.replicate 2 (-1, 0)
         , 0.1
         )

bautin :: NeurOscSimPars
bautin = ( freq
         , -0.4
         , V.cons (1.2, -0.01) $ V.singleton (-1, 0)
         , 0.1
         )

-- plot parameters
type NeurOscPlotPars = (AlphaColour Double, AlphaColour Double)

neurOscPlotPars :: NeurOscPlotPars
neurOscPlotPars = (opaque rosybrown, opaque rosybrown)


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- plot simulation results
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
plotSimResults :: String ->
                  ((AlphaColour Double, AlphaColour Double), (AlphaColour Double, AlphaColour Double)) ->
                  (([(Double, Double)], [(Double, Double)]), ([(Double, Double)], [(Double, Double)])) ->
                  IO ()
plotSimResults fileName (inputOscColours, neurOscColours) simResults
  = void $ renderableToPNGFile (renderLayout1sStacked . map withAnyOrdinate . uncurry (++) .
                                (plotOscResults inputOscColours *** plotOscResults neurOscColours) $ simResults
                               )
                               400 600 (fileName ++ ".png") -- 1280 730

----------------------------------------------------------------------------------------------------
-- plot results for oscillator
----------------------------------------------------------------------------------------------------
plotOscResults :: (AlphaColour Double, AlphaColour Double) -> ([(Double, Double)], [(Double, Double)]) ->
                  [Layout Double Double]
plotOscResults (amplitudeColour, freqColour) (amplitude, freq)
  = [plotOscAmplitude amplitudeColour amplitude, plotOscFreq freqColour freq]


-- plot intantaneous amplitude of oscillator
plotOscAmplitude :: AlphaColour Double -> [(Double, Double)] -> Layout Double Double
plotOscAmplitude colour values
  = layout1_bottom_axis ^= (laxis_title   ^= "time (s)"    $ defaultLayoutAxis)
  $ layout1_top_axis    ^= (laxis_visible ^= const False $ defaultLayoutAxis)
  $ layout1_left_axis   ^= (laxis_title   ^= "amplitude"   $ defaultLayoutAxis)
  $ layout1_right_axis  ^= (laxis_visible ^= const False $ defaultLayoutAxis)
  $ layout1_plots       ^= [ Left . toPlot $
                             (  plot_lines_style  .> line_color ^= colour
                              $ plot_lines_values ^= [values]
                              $ defaultPlotLines
                             )
                           ]
  $ defaultLayout1

-- plot instantaneous frequency of oscillator
plotOscFreq :: AlphaColour Double -> [(Double, Double)] -> Layout Double Double
plotOscFreq colour values
  = layout1_bottom_axis ^= (laxis_title   ^= "time (s)"       $ defaultLayoutAxis)
  $ layout1_top_axis    ^= (laxis_visible ^= const False    $ defaultLayoutAxis)
  $ layout1_left_axis   ^= (laxis_title   ^= "frequency (Hz)" $ defaultLayoutAxis)
  $ layout1_right_axis  ^= (laxis_visible ^= const False    $ defaultLayoutAxis)
  $ layout1_plots       ^= [ Left . toPlot $
                             (  plot_lines_style  .> line_color ^= colour
                              $ plot_lines_values ^= [values]
                              $ defaultPlotLines
                             )
                           ]
  $ defaultLayout1

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- collect simulation results
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
getSimResults :: (InputOscillation, NeurOscillation) ->
                 (([(Double, Double)], [(Double, Double)]), ([(Double, Double)], [(Double, Double)]))
getSimResults = getInputOscResults *** getNeurOscResults

----------------------------------------------------------------------------------------------------
-- collect simulation results for input oscillator
----------------------------------------------------------------------------------------------------
getInputOscResults :: InputOscillation -> ([(Double, Double)], [(Double, Double)])
getInputOscResults = getInputOscAmplitude &&& getInputOscFreq

-- get instantaneous amplitude of input oscillator
getInputOscAmplitude :: InputOscillation -> [(Double, Double)]
getInputOscAmplitude = ((0, 3) :) .  map (\(t, ((rX, _), _)) -> (t, rX))

-- get instantaneous frequency of input oscillator
getInputOscFreq :: InputOscillation -> [(Double, Double)]
getInputOscFreq inputOsc
  = (0, 0.5 * freq) : (0, 2 * freq) : map (\(t, (_, inputOscFreq)) -> (t, inputOscFreq)) inputOsc

----------------------------------------------------------------------------------------------------
-- collect simulation results for neural oscillator
----------------------------------------------------------------------------------------------------
getNeurOscResults :: NeurOscillation -> ([(Double, Double)], [(Double, Double)])
getNeurOscResults = getNeurOscAmplitude &&& getNeurOscFreq

-- get instantaneous amplitude of neural oscillator
getNeurOscAmplitude :: NeurOscillation -> [(Double, Double)]
getNeurOscAmplitude neurOsc = (0, 3) : map (\(t, ((rZ, _), _)) -> (t, rZ)) neurOsc

-- get instantaneous frequency of neural oscillator
getNeurOscFreq :: NeurOscillation -> [(Double, Double)]
getNeurOscFreq neurOsc
  = filter (\(t, f) -> f >= 0.5 * freq && f <= 2 * freq)
           ((0, 0.5 * freq) : (0, 2 * freq) : map (\(t, (_, (_, phiZ'))) -> (t, phiZ' / (2 * pi))) neurOsc)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- run simulation
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
runSim :: (InputOscSimPars, NeurOscSimPars) -> [Double] -> (InputOscillation, NeurOscillation)
runSim (inputOscSimPars, neurOscSimPars) ts = (inputOsc, runNeurOsc neurOscSimPars inputOsc)
    where inputOsc = runInputOsc inputOscSimPars ts

----------------------------------------------------------------------------------------------------
-- generate input to oscillator
----------------------------------------------------------------------------------------------------
type InputOscillation = [(Double, ((Double, Double), Double))]

runInputOsc :: InputOscSimPars -> [Double] -> InputOscillation
runInputOsc (r1', r2', rmax, ton, toff, freq) ts
  = zip ts . zip inputOsc $ map (const freq) inputOsc
    where inputOsc = map (((-) <$> r r1' rmax ton <*> r r2' rmax toff) &&& (`phi` freq)) ts

----------------------------------------------------------------------------------------------------
-- simulate neural oscillation
----------------------------------------------------------------------------------------------------
type NeurOscillation = [(Double, ((Double, Double), (Double, Double)))]

runNeurOsc :: NeurOscSimPars -> InputOscillation -> NeurOscillation
runNeurOsc pars
  = scanl (\(t1, ((rZ, phiZ), _)) (t2, (x, _)) ->
             let (rZ', phiZ') = oscEq pars (rZ, phiZ) x
                 dt           = t2 - t1
             in  (t2, (polar $ mkPolar (rZ + dt * rZ') (phiZ + dt * phiZ'), (rZ', phiZ')))
          )
          (0, ((1e-2, 0), (0, 0)))
