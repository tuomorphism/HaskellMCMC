module MCMC.Sample (GibbConfiguration (..), GibbStrategy) where

import MCMC.Types (Distribution, Value)
import Numeric.Natural

data GibbStrategy = Random | Deterministic

data GibbConfiguration = GibbConfiguration {strategy :: GibbStrategy, iterations :: Natural}

sampleDeterministicGibbs :: Distribution -> Natural -> Distribution
sampleDeterministicGibbs u n = Distribution

gibbSampler :: Distribution -> GibbStrategy -> Distribution
gibbSampler u (GibbConfiguration strat iters) =
  case strat of
    Deterministic -> sampleDeterministicGibbs u strat
