module Simulation where


import Types
import Lens.Micro

-- | Main Simulation Engine, runs one step of Simulation, changing all state
simulator :: Simulation -> Simulation
simulator s = case _nextEvent s of
  StopSimulation  -> s                      -- if next Event is StopSimulation then just return        
  CustomerArrival -> s'                     -- how to update if next event is customer arrival event
    where
      s'  = s 
        & bankState .~ bs'
        & clockTime .~ (snd $ getNextEventAndTime bs')
        & nextEvent .~ (fst $ getNextEventAndTime bs')
        & interArrivalTimes %~ drop 1
        & procTimes %~ drop 1

      bs' = bs
        & currQueueLength     .~ newQueueLength
        & maxQueueLength      %~ (max newQueueLength)
        & queueLengthSum      %~ (+ newQueueLength)
        & queueWaitTime       .~ newQueueWaitTime
        & waitTimeSum         %~ (+ newQueueWaitTime)
        & maxWaitTime         %~ (max newCustWaitTime)
        & waitingCustomers    .~ newQueue
        & nextArrivalAt       .~ nextArrivalClockTime
        & nextServerIdleAt    %~ changeServerIdleClockTime
        & numCustomersArrived %~ succ

      bs = s ^. bankState
      newQueueLength = case isServerBusy of
        True -> bs ^. currQueueLength + 1
        _    -> 0
      newQueueWaitTime = case isServerBusy of
        True -> bs ^. queueWaitTime  + newCustProcTime
        _    -> 0
      newCustWaitTime = case isServerBusy of
        True  -> bs ^. queueWaitTime  + (bs ^. nextServerIdleAt - s ^. clockTime)
        False -> 0
      
      newQueue = case isServerBusy of
        True -> bs ^. waitingCustomers <> [Customer newCustProcTime]
        _    ->   bs ^. waitingCustomers
      nextArrivalClockTime = s ^. clockTime + (s ^. interArrivalTimes & head)
      newCustProcTime = s ^. procTimes & head
      changeServerIdleClockTime = case newQueueLength of
        0 -> const (s ^. clockTime + newCustProcTime)
        _  -> id
      isServerBusy = (bs ^. nextServerIdleAt) > (s ^. clockTime)

  ServeCustomer -> s'                     -- how to update if next event is customer arrival event
    where
      s' = s 
        & bankState .~ bs'
        & clockTime .~ (snd $ getNextEventAndTime bs')
        & nextEvent .~ (fst $ getNextEventAndTime bs')

      bs' = bs
        & currQueueLength  %~ (\x -> if x < 1 then 0 else pred x)
        & queueWaitTime    .~ newQueueWaitTime 
        & waitingCustomers .~ newQueue
        & nextServerIdleAt .~ nextServerIdleClockTime

      bs = s ^. bankState
      nextServerIdleClockTime = s ^. clockTime + custProcTime
      custGettingServed       = bs ^. waitingCustomers & head
      custProcTime            = custGettingServed ^. processingTime
      newQueueWaitTime        = case newQueue of
        [] -> 0
        _  -> bs ^. queueWaitTime - custProcTime
      newQueue = drop 1 $ bs ^. waitingCustomers

  where
    getNextEventAndTime :: BankState -> (SimEvent, Time)
    getNextEventAndTime bs
      | bs ^. numCustomersArrived >= s ^. totalNumCustomers && bs ^. waitingCustomers == [] = (StopSimulation, (bs ^. nextServerIdleAt))
      | otherwise =
        case compare (bs ^. nextArrivalAt) (bs ^. nextServerIdleAt) of
          LT -> (CustomerArrival, bs ^. nextArrivalAt)
          _ | bs ^. currQueueLength == 0 -> (CustomerArrival, bs ^. nextArrivalAt)
          _  -> (ServeCustomer, bs ^. nextServerIdleAt)


initBankState :: BankState
initBankState = BankState {
    _currQueueLength     = 0
  , _maxQueueLength      = 0
  , _queueLengthSum      = 0
  , _queueWaitTime       = 0
  , _waitTimeSum         = 0
  , _maxWaitTime         = 0
  , _waitingCustomers    = []
  , _nextArrivalAt       = 0
  , _nextServerIdleAt    = 0
  , _numCustomersArrived = 0
  }
    
