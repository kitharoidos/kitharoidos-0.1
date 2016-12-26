{-#LANGUAGE NamedFieldPuns#-}

module Main (
    -- * Simulate a network of neural oscillators.
      main
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

main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> args)
             ( fullDesc
            <> progDesc "Simulate network of neural oscillators listening to MIDI music."
            <> header "kitharoidos"
             )

data Args = Args
    { g :: Bool
    , s :: Bool
    }

args :: Parser Args
args = Args
    <$> switch
         ( long "test-input-generator"
        <> short 'g'
        <> help "Plot amplitude envelope and phase of a 6s oscillation."
         )
    <*> switch
         ( long "test-gfnn-simulator"
        <> short 's'
        <> help "Simulate neural oscillation with a single canonical oscillator poised in three different regimes: critical, active, and bautin."
         )

run :: Args -> IO ()
run args = do
    when (g args) testInputGenerator
    when (s args) testGFNNSimulator
    simulateGFNN

-- | Time points for subsequent simulations.
ts :: V.Vector Double
ts = V.enumFromStepN 0 1e-4 60000

-- | Simulate a network of neural oscillators listening to MIDI music
simulateGFNN :: IO ()
simulateGFNN = runKitharoidos experiment0

-- | Test generator of input oscillations
testInputGenerator :: IO ()
testInputGenerator = do toFile def ("envelope" ++ ".svg") plotEnvelope
                        toFile def ("phase"    ++ ".svg") plotPhase

-- | Plot amplitude envelope of a 6s oscillation.
plotEnvelope :: EC (Layout Double Double) ()
plotEnvelope
  = do layout_x_axis . laxis_title .= "time (s)"
       layout_y_axis . laxis_title .= "amplitude"
       plot . liftEC $ do
           plot_lines_style  . line_color .= opaque green
           plot_lines_values .= [(0,  1.5) : V.toList (V.zip ts (V.map (r 1 1 1) ts))]
       plot . liftEC $ do
           plot_lines_style  . line_color .= opaque red
           plot_lines_values .= [(0, -1.5) : V.toList (V.zip ts (V.map ((* (-1)) . r 1 1 3) ts))]

-- | Plot phase of a 6s oscillation.
plotPhase :: EC (Layout Double Double) ()
plotPhase
  = do layout_x_axis . laxis_title .= "time (s)"
       layout_y_axis . laxis_title .= "phase (rad)"
       plot . liftEC $ do
           plot_lines_style  . line_color .= opaque cadetblue
           plot_lines_values .= [V.toList $ V.zip ts (V.map (`phi` 1) ts)]

-- | Simulate neural oscillation with a single canonical oscillator poised in three different regimes.
testGFNNSimulator :: IO ()
testGFNNSimulator = do simulate "critical" pars1 ts
                       simulate "active"   pars2 ts
                       simulate "bautin"   pars3 ts

-- | Simulate neural oscillation.
simulate :: String -> Main.Pars -> V.Vector Double -> IO ()
simulate fileName Main.Pars {simPars, plotPars} ts
  = plotSimResults fileName plotPars . getSimResults $ runSim simPars ts

-- | Autonomous frequency of neural oscillation (Middle C).
freq :: Double
freq = 261.6255653006

-- | Data type for storing simulation parameters.
data Pars = Pars { simPars  :: (InputOscSimPars , NeurOscSimPars)
                 , plotPars :: (InputOscPlotPars, NeurOscPlotPars)
                 }

-- | Parameters for the three simulations.
pars1 :: Main.Pars
pars1 = Main.Pars {simPars = (rectangle, critical)    , plotPars = (inputOscPlotPars, neurOscPlotPars)}

pars2 :: Main.Pars
pars2 = Main.Pars {simPars = (rectangle, active)      , plotPars = (inputOscPlotPars, neurOscPlotPars)}

pars3 :: Main.Pars
pars3 = Main.Pars {simPars = (rectangle, bautin)      , plotPars = (inputOscPlotPars, neurOscPlotPars)}

-- | Data type for storing parameters of input oscillation.
type InputOscSimPars = (Double, Double, Double, Double, Double, Double)

-- | Rectangular amplitude envelope to be used in the simulations.
rectangle :: InputOscSimPars
rectangle = (r', r', rmax, 2, 4, freq)
    where r'   = rmax / 1e-3
          rmax = 1

-- | Data type for storing parameters of graphical rendering of input oscillation.
type InputOscPlotPars = (AlphaColour Double, AlphaColour Double)

-- | Parameters to be used for plotting input oscillation in the simulations.
inputOscPlotPars :: InputOscPlotPars
inputOscPlotPars = (opaque cadetblue, opaque cadetblue)

-- | Data type for storing parameters of neural oscillator.
type NeurOscSimPars = (Double, Double, V.Vector (Double, Double), Double)

-- | With these parameter values, the oscillator is damped.
critical :: NeurOscSimPars
critical = ( freq
           , 0
           , V.replicate 2 (-1, 0)
           , 0.1
           )

-- | With these parameter values, the oscillator is undamped.
active :: NeurOscSimPars
active = ( freq
         , 0.1
         , V.replicate 2 (-1, 0)
         , 0.1
         )

-- | With these parameter values, there is a bistability of a fixed point and a limit cycle.
bautin :: NeurOscSimPars
bautin = ( freq
         , -0.4
         , V.cons (1.2, -0.01) $ V.singleton (-1, 0)
         , 0.1
         )

-- | Parameters for graphical rendering of neural oscillation.
type NeurOscPlotPars = (AlphaColour Double, AlphaColour Double)

-- | Parameters to be used for plotting neural oscillation in the simulations.
neurOscPlotPars :: NeurOscPlotPars
neurOscPlotPars = (opaque rosybrown, opaque rosybrown)

-- | Plot simulation results
plotSimResults :: String ->
                  ((AlphaColour Double, AlphaColour Double), (AlphaColour Double, AlphaColour Double)) ->
                  ((V.Vector (Double, Double), V.Vector (Double, Double)), (V.Vector (Double, Double), V.Vector (Double, Double))) ->
                  IO ()
plotSimResults fileName (inputOscColours, neurOscColours) simResults
  = toFile def (fileName ++ ".svg") $
      slayouts_layouts .= (uncurry (++) . (plotOsc inputOscColours *** plotOsc neurOscColours) $ simResults)

-- | Plot oscillation.
plotOsc :: (AlphaColour Double, AlphaColour Double) -> (V.Vector (Double, Double), V.Vector (Double, Double)) ->
           [StackedLayout Double]
plotOsc (amplitudeColour, freqColour) (amplitude, freq)
  = StackedLayout . execEC <$> [plotOscAmplitude amplitudeColour amplitude, plotOscFreq freqColour freq]


-- | Plot intantaneous amplitude of oscillation.
plotOscAmplitude :: AlphaColour Double -> V.Vector (Double, Double) -> EC (Layout Double Double) ()
plotOscAmplitude colour values
  = do layout_x_axis . laxis_title .= "time (s)"
       layout_y_axis . laxis_title .= "amplitude"
       plot . liftEC $ do plot_lines_style . line_color .= colour
                          plot_lines_values .= [V.toList values]

-- | Plot instantaneous frequency of oscillation.
plotOscFreq :: AlphaColour Double -> V.Vector (Double, Double) -> EC (Layout Double Double) ()
plotOscFreq colour values
  = do layout_x_axis . laxis_title .= "time (s)"
       layout_y_axis . laxis_title .= "frequency (Hz)"
       plot . liftEC $ do plot_lines_style  . line_color .= colour
                          plot_lines_values .= [V.toList values]

-- | Collect simulation results.
getSimResults :: (InputOscillation, NeurOscillation) ->
                 ((V.Vector (Double, Double), V.Vector (Double, Double)), (V.Vector (Double, Double), V.Vector (Double, Double)))
getSimResults = getInputOscResults *** getNeurOscResults

-- | Collect simulation results for input oscillation.
getInputOscResults :: InputOscillation -> (V.Vector (Double, Double), V.Vector (Double, Double))
getInputOscResults = getInputOscAmplitude &&& getInputOscFreq

-- | Get instantaneous amplitude of input oscillation.
getInputOscAmplitude :: InputOscillation -> V.Vector (Double, Double)
getInputOscAmplitude = V.fromList . ((0, 3) :) .  map (\(t, ((rX, _), _)) -> (t, rX)) . V.toList

-- | Get instantaneous frequency of input oscillation.
getInputOscFreq :: InputOscillation -> V.Vector (Double, Double)
getInputOscFreq inputOsc
  = V.fromList $ (0, 0.5 * freq) : (0, 2 * freq) : map (\(t, (_, inputOscFreq)) -> (t, inputOscFreq)) inputOsc'
    where inputOsc' = V.toList inputOsc

-- | Collect simulation results for neural oscillation.
getNeurOscResults :: NeurOscillation -> (V.Vector (Double, Double), V.Vector (Double, Double))
getNeurOscResults = getNeurOscAmplitude &&& getNeurOscFreq

-- | Get instantaneous amplitude of neural oscillation.
getNeurOscAmplitude :: NeurOscillation -> V.Vector (Double, Double)
getNeurOscAmplitude neurOsc = V.fromList $ (0, 3) : map (\(t, ((rZ, _), _)) -> (t, rZ)) neurOsc'
    where neurOsc' = V.toList neurOsc

-- | Get instantaneous frequency of neural oscillation.
getNeurOscFreq :: NeurOscillation -> V.Vector (Double, Double)
getNeurOscFreq neurOsc
  = V.fromList $ filter (\(t, f) -> f >= 0.5 * freq && f <= 2 * freq)
                        ((0, 0.5 * freq) : (0, 2 * freq) : map (\(t, (_, (_, phiZ'))) -> (t, phiZ' / (2 * pi))) neurOsc')
    where neurOsc' = V.toList neurOsc

-- | Run simulation
runSim :: (InputOscSimPars, NeurOscSimPars) -> V.Vector Double -> (InputOscillation, NeurOscillation)
runSim (inputOscSimPars, neurOscSimPars) ts = (inputOsc, runNeurOsc neurOscSimPars inputOsc)
    where inputOsc = runInputOsc inputOscSimPars ts

-- | Input oscillation.
type InputOscillation = V.Vector (Double, ((Double, Double), Double))

-- | Generate input oscillation.
runInputOsc :: InputOscSimPars -> V.Vector Double -> InputOscillation
runInputOsc (r1', r2', rmax, ton, toff, freq) ts
  = V.zip ts . V.zip inputOsc $ V.map (const freq) inputOsc
    where inputOsc = V.map (((-) <$> r r1' rmax ton <*> r r2' rmax toff) &&& (`phi` freq)) ts

-- | Neural oscillation.
type NeurOscillation = V.Vector (Double, ((Double, Double), (Double, Double)))

-- | Simulate neural oscillation.
runNeurOsc :: NeurOscSimPars -> InputOscillation -> NeurOscillation
runNeurOsc pars
  = V.scanl' (\(t1, ((rZ, phiZ), _)) (t2, (x, _)) ->
                let (rZ', phiZ') = oscEq pars (rZ, phiZ) x
                    dt           = t2 - t1
                in  (t2, (polar $ mkPolar (rZ + dt * rZ') (phiZ + dt * phiZ'), (rZ', phiZ')))
             )
             (0, ((1e-2, 0), (0, 0)))
