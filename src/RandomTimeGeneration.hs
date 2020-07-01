module RandomTimeGeneration where


import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad

import Types

-- | Generate random Inter Arrival Times with exponential distribution
--   and scale as 100
genInterArrivalTimes :: Int -> IO [Time]
genInterArrivalTimes n = do
    gen <- create
    xs <- replicateM n $ exponential 100 gen
    return $ xs

-- | Generate Processing Times for given alpha beta with p fixed as 200
genProcessingTimes :: Double -> Double -> Int -> IO [Time]
genProcessingTimes alpha beta count = do
  gen <- create
  randVals <- replicateM count $ uniform gen
  return $ toProcessingTime alpha beta <$> randVals
  where
    toProcessingTime :: Double -> Double -> Double -> Double
    toProcessingTime a b x = 200 * x ** (a - 1) * (1 - x) ** (b - 1)

-- | Generate Processing Times for given customer type
genCustProcTimes :: Int -> CustomerType -> IO [Double]
genCustProcTimes n ct = case ct of
  Yellow -> genProcessingTimes 2 5 n
  Red    -> genProcessingTimes 2 2 n
  Blue   -> genProcessingTimes 5 1 n