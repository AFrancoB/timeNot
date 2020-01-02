module Sound.TimeNot.InfinityFuncs (infinitizar) where

import Data.Time
import Control.Lens

import Sound.TimeNot.AST

today = fromGregorian 2019 07 18
myUTCTest1 = UTCTime today 30

myUTCTest2 = UTCTime today 0
now = myUTCTest2

--data Event  = Event {time :: UTCTime} deriving (Show)
--canon :: [(NominalDiffTime, String)]
--canon :: [Event]
--canon = [Event {time = myUTCTest1}]

toNominalDiffTime n = n :: NominalDiffTime
canonDuration = toNominalDiffTime 10000000000000

--updateEventTime :: (Monad m, Real a) => m UTCTime -> a -> a -> m UTCTime
updateEventTime:: UTCTime -> UTCTime -> NominalDiffTime -> UTCTime
updateEventTime now time nextCanonStartTime = addUTCTime (diffUTCTime (addUTCTime nextCanonStartTime time) now) now 

infinitizar:: UTCTime -> Double -> [Event] -> [Event] 
infinitizar now canonDur canon =
  let canonDur' = realToFrac (canonDur*1000000000000) :: NominalDiffTime
  in concat $ zipWith (\canon' nextCanonStartTime -> (func1 canon' now nextCanonStartTime ))
     (repeat canon) 
     (map (canonDur'*) [0..])

getTotalDur :: [Event] -> NominalDiffTime
getTotalDur es = realToFrac (sum $ map lengthEvent es)
  
func1 canon now nextCanonStartTime = 
  map 
    (\event -> event {time = updateEventTime now (time event) nextCanonStartTime})
    canon



--eventTest = [Event {time = UTCTime 2020-01-02 00:00:00, lengthEvent = 0.5, pitch = 60.0, instrument = "saw", amp = 0.16666666666666666},Event {time = UTCTime 2020-01-02 00:00:00.5, lengthEvent = 0.5, pitch = 62.0, instrument = "saw", amp = 0.16666666666666666}]
