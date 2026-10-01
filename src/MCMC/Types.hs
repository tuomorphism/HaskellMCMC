module MCMC.Types (Value, Distribution (..), Vec2 (..), Mat2 (..), mkMat2, mkVec2, mkDistribution) where

import Numeric.Natural

type Value = Double

-- A vector v has elements x and y.
data Vec2 = Vec2 {x :: Value, y :: Value}

-- A matrix of Mat2 has columns u and v.
data Mat2 = Mat2 {u :: Vec2, v :: Vec2}

instance Show Vec2 where
  show (Vec2 x y) = show "Vec2{x=" ++ show x ++ ",y=" ++ show y ++ "}"

instance Show Mat2 where
  show (Mat2 u v) = "Mat2{col1=" ++ show u ++ ",col2=" ++ show v ++ "}"

mkVec2 :: Value -> Value -> Vec2
mkVec2 x y = Vec2 {x = x, y = y}

scaleVec2 :: Value -> Vec2 -> Vec2
scaleVec2 s (Vec2 x y) = mkVec2 (s * x) (s * y)

zeroVec2 :: Vec2
zeroVec2 = Vec2 {x = 0, y = 0}

mkMat2 :: Vec2 -> Vec2 -> Mat2
mkMat2 u v = Mat2 u v

-- Simple matrix operations
scaleMat2 :: Value -> Mat2 -> Mat2
scaleMat2 s (Mat2 u v) = mkMat2 (scaleVec2 s u) (scaleVec2 s v)

detMat2 :: Mat2 -> Value
detMat2 (Mat2 (Vec2 a c) (Vec2 b d)) = a * d - b * c

adjugateMat2 :: Mat2 -> Mat2
adjugateMat2 (Mat2 (Vec2 a c) (Vec2 b d)) = mkMat2 (mkVec2 d (-c)) (mkVec2 (-b) a)

invMat2 :: Mat2 -> Mat2
invMat2 m = scaleMat2 (1.0 / detM) adjM
  where
    adjM = adjugateMat2 m
    detM = detMat2 m

-- Distribution refers to a 2D gaussian, which is uniquely characterized by the expectation vector and covariance matrix
data Distribution = Distribution
  { mean :: Vec2,
    cov :: Mat2,
    d :: Natural,
    prc :: Mat2
  }

mkDistribution :: Vec2 -> Mat2 -> Distribution
mkDistribution mean cov = Distribution {mean = mean, cov = cov, d = 2, prc = invMat2 cov}
