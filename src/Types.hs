{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH
import Lens.Micro

type Time = Double


data Customer = Customer {
    _processingTime :: Time -- ^ Processing time required for this customer
  } deriving (Eq)

makeLenses ''Customer

instance Show Customer where
  show c = "C " <> show (c ^. processingTime) 


data BankState = BankState {
    _currQueueLength     :: Int         -- ^ current length of queue
  , _maxQueueLength      :: Int         -- ^ maximum length of queue till now
  , _queueLengthSum      :: Int         -- ^ sum of all queue lengths till now 
  , _queueWaitTime       :: Time        -- ^ sum of wait time based on current queue
  , _waitTimeSum         :: Time        -- ^ sum of all waiting time till now
  , _maxWaitTime         :: Time        -- ^ maximum wait time for any customer
  , _waitingCustomers    :: [Customer]  -- ^ customers waiting in queue
  , _nextArrivalAt       :: Time        -- ^ clock time of next customer arrival
  , _nextServerIdleAt    :: Time        -- ^ clock time when server will be free
  , _numCustomersArrived :: Int         -- ^ number of customers arrived at bank till now
  } deriving (Eq)

makeLenses ''BankState

instance Show BankState where
  show bs = unlines info
    where
      info = [ ""
              ,"currQueueLength: " <> show (bs ^. currQueueLength) 
                 <> "\t" <> "maxQueueLength: " <> show  (bs ^. maxQueueLength) 
                 <> "\t" <> "queueLengthSum: " <> show ( bs ^. queueLengthSum)
                 <> "\t" <> "queueWaitTime: " <> show ( bs ^. queueWaitTime)
              ,
               "waitTimeSum: " <> show ( bs ^. waitTimeSum)
                 <> "\t" <> "maxWaitTime: " <> show ( bs ^. maxWaitTime)
              ,
               "waiting Customers: " <> show (bs ^. waitingCustomers)
              , 
               "Next Arrival At: " <> show ( bs ^. nextArrivalAt)
              ,
               "NextServerIdleAt: " <> show ( bs ^. nextServerIdleAt)
              ,
               "Num Of Customers Arrived: " <> show (bs ^. numCustomersArrived)
             ]


data CustomerType =
    Yellow
  | Red
  | Blue
  deriving (Show, Eq)

data SimEvent =
    CustomerArrival
  | ServeCustomer
  | StopSimulation
  deriving (Show, Eq)

data Simulation = Simulation {
    _bankState         :: BankState
  , _clockTime         :: Time
  , _nextEvent         :: SimEvent
  , _procTimes         :: [Time]
  , _interArrivalTimes :: [Time]
  , _totalNumCustomers :: Int
  }  deriving (Eq)

makeLenses ''Simulation

instance Show Simulation where
  show s = unlines info
    where
      info = [ ""
             ,
              "--------------- Simulation State ---------------"
             , show $ s ^. bankState
             , "Clock Time: " <> show (s ^. clockTime)
             , "Next Event: " <> show (s ^. nextEvent)
             , "Next 4 InterArrivalTimes:" <> show (take 4 $ s ^. interArrivalTimes)
             , "Next 4 ProcessingTimes:" <> show (take 4 $ s ^. procTimes)
             , "Total Cust Count: " <> show (s ^. totalNumCustomers)
             , "---------------  -------------- ---------------"
             ]