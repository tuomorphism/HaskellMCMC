module MCMC.Sample (GibbConfiguration (..)) where

import MCMC.Types (Distribution, Mat2, Value, mkDistribution)
import Numeric.Natural

data GibbConfiguration = GibbConfiguration {iterations :: Natural}

gaussianConjugate :: Distribution -> Mat2 -> [Value] -> Distribution
gaussianConjugate meanPriorDist priorCov x = mkDistribution informationMean informationCov
