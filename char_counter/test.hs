{-# LANGUAGE DeriveGeneric #-}
module CoverageAnalysis (calculateSuspiciousness ) where
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Trace.Hpc.Tix (writeTix, readTix)
import Trace.Hpc.Mix (readMix)

import qualified Data.Map as Map

-- Calculate the suspiciousness score for a line of code using the Tarantula formula
tarantula :: Int -> Int -> Int -> Int -> Double
tarantula c f p nf = numerator / denominator
  where numerator = fromIntegral c / (fromIntegral c + fromIntegral p)
        denominator = (fromIntegral nf / (fromIntegral nf + fromIntegral p)) +
                      (fromIntegral c / (fromIntegral c + fromIntegral f))

-- Read the coverage data from a file and calculate the suspiciousness scores
calculateSuspiciousness :: FilePath -> IO ()
calculateSuspiciousness file = do
  -- Parse the coverage data
    tix <- readFile file
    putStrLn (show tix)