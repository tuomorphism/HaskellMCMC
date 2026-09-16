module Main (main) where

import Data.Semiring (times)
import FaithfulData (waiting)
import MCMC.Log

main :: IO ()
main = do
  putStrLn $ show (head waiting)
