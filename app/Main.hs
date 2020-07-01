
module Main where

import Simulation
import Types
import RandomTimeGeneration
import Lens.Micro
import Data.List (sortOn)

main :: IO ()
main = giveAnswers 10000


giveAnswers :: Int -> IO ()
giveAnswers n = do
  resSim1 <- runSimulation n Yellow
  resSim2 <- runSimulation n Red
  resSim3 <- runSimulation n Blue
  let results = [resSim1, resSim2, resSim3]
  putStrLn "-------------------- Simulation Results --------------------"
  putStrLn "Given only YELLOW customers :-" 
  putStrLn $ "  Average customer waiting time: " <> show (avgCustWaitTime resSim1)
  putStrLn $ "  Maximum customer waiting time: " <> show (maxCustWaitTime resSim1)
  putStrLn "Given only RED customers :-" 
  putStrLn $ "  Average queue length in-front of the teller: " <> show (avgCustQueueLength resSim2)
  putStrLn $ "  Maximum queue length in-front of the teller: " <> show (maxCustQueueLength resSim2)
  putStrLn $ "The type of customer that gives the closest value between"
  putStrLn $ "the average and maximum customer waiting times is: " <> show (fastestCtype results)
  where
    avgCustWaitTime :: Simulation -> Time
    avgCustWaitTime s = (s ^. bankState ^. waitTimeSum) / (fromIntegral $ s ^. totalNumCustomers)

    maxCustWaitTime :: Simulation -> Time
    maxCustWaitTime s = s ^. bankState ^. maxWaitTime
    
    avgCustQueueLength :: Simulation -> Time
    avgCustQueueLength s = fromIntegral (s ^. bankState ^. queueLengthSum) / (fromIntegral $ s ^. totalNumCustomers)

    maxCustQueueLength :: Simulation -> Int
    maxCustQueueLength s = s ^. bankState ^. maxQueueLength

    fastestCtype results = fst . head . sortOn snd $ 
      zipWith (\s ct ->(ct, maxCustWaitTime s - avgCustWaitTime s)) results ctypes
    ctypes = [Yellow, Red, Blue]

-- | Run Simulation for given number of customer of a particular type
--   returns the Resultant Simulation state
runSimulation :: Int -> CustomerType -> IO Simulation
runSimulation n ct = do
  aTime : arrTimes <- genInterArrivalTimes (n + 2)
  pTime : procTimes <- genCustProcTimes (n + 2) ct
  let simInitState = 
        Simulation
          (initBankState & nextArrivalAt .~ aTime)
          0
          CustomerArrival
          procTimes
          arrTimes
          n
      simResultState = (iterate simulator simInitState) !! n
  return simResultState
