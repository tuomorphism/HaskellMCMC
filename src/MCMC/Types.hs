module MCMC.Types (Value, Distribution (..), Vec2 (..), Mat2 (..), mkMat2, mkVec2) where

import Numeric.Natural

type Value = Double

data Vec2 = Vec2 {x :: Value, y :: Value}

data Mat2 = Mat2 {col :: [Vec2]}

instance Show Vec2 where
  show (Vec2 x y) = show "Vec2{x=" ++ show x ++ ",y=" ++ show y ++ "}"

instance Show Mat2 where
  show (Mat2 [col1, col2]) = "Mat2{col1=" ++ show col1 ++ ",col2=" ++ show col2 ++ "}"
  show (Mat2 []) = "Empty Mat2"
  show (Mat2 [x]) = "Mat2{col1=" ++ show x ++ "}"
  show (Mat2 (_ : _ : _)) = "Malformed Mat2"

mkVec2 :: Value -> Value -> Vec2
mkVec2 x y = Vec2 {x = x, y = y}

zeroVec2 :: Vec2
zeroVec2 = Vec2 {x = 0, y = 0}

mkMat2 :: [Vec2] -> Mat2
mkMat2 [col1, col2] = Mat2 {col = [col1, col2]}
mkMat2 [] = Mat2 {col = [zeroVec2, zeroVec2]}
mkMat2 [x] = Mat2 {col = [x, zeroVec2]}
mkMat2 (_ : _ : _) = Mat2 {col = [zeroVec2, zeroVec2]} -- Giving zeros if malformed

-- Simple matrix operations
invMat2 :: Mat2 -> Mat2
invMat2 m = scaleMat2 $ conM (1.0 / detM)
  where
    conM = conjugateMat2 m
    detM = determinantMat2 m

scaleMat2 :: Mat2 -> Value -> Mat2
scaleMat2 ((Mat2 [(Vec2 x1 y1), (Vec2 x2 y2)])) v = mkMat2 [mkVec2 ((v * x1) (v * y1)), mkVec2 ((v * x2) (v * y2))]

determinantMat2 :: Mat2 -> Value
determinantMat2 (Mat2 [(Vec2 x1 y1), (Vec2 x2 y2)]) = x1 * y2 - x2 * y1

conjugateMat2 :: Mat2 -> Mat2
conjugateMat2 (Mat2 [(Vec2 x1 y1), (Vec2 x2 y2)]) = mkMat2 [Vec2 {x = y2, y = -x2}, Vec2 {x = -y1, y = x1}]

-- Distribution refers to a 2D gaussian, which is uniquely characterized by the expectation vector and covariance matrix
data Distribution = Distribution
  { mean :: Vec2,
    cov :: Mat2,
    d :: Natural,
    prc :: Mat2
  }

mkDistribution :: Vec2 -> Mat2 -> Distribution
mkDistribution mean cov = Distribution {mean = mean, cov = cov, d = 2, prc = invMat2 cov}
