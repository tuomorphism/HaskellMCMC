module MCMC.Log (Log (..)) where

import Data.Semiring (Semiring (..))
import GHC.Float (log1pexp)
import MCMC.Types (Value)

newtype Log a = Log {ln :: a} deriving (Eq, Ord)

instance Semiring (Log Value) where
  plus = logPlus
  times = \(Log x) (Log y) -> Log (x + y)
  zero = Log (-1 / 0)
  one = Log 0

logPlus :: Log Value -> Log Value -> Log Value
logPlus (Log x) (Log y)
  | isInfinite x, x < 0 = Log y
  | isInfinite y, y < 0 = Log x
  | x >= y = Log (x + log1pexp (y - x))
  | otherwise = Log (y + log1pexp (x - y))

instance (Show a, Floating a) => Show (Log a) where
  show (Log x) = "exp(" ++ show x ++ ")"
