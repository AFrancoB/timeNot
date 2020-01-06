module Sound.TimeNot.InfinityFuncs (infinitizar) where

import Data.Time
import Control.Lens

import Sound.TimeNot.AST


infinitizar:: UTCTime -> Double -> [Event] -> [Event] 
infinitizar evalT canonDur events =
  let canonDur' = realToFrac (canonDur*1000000000000) :: NominalDiffTime
  in concat $ zipWith (\canon nextCanonStartTime -> (infTimesToCanon canon evalT nextCanonStartTime ))
     (repeat events) 
     (map (canonDur'*) [0..])

infTimesToCanon:: [Event] -> UTCTime -> NominalDiffTime -> [Event]
infTimesToCanon events evalT nextCanonStartTime = map (\event -> event {time = updateEventTime evalT (time event) nextCanonStartTime}) events

updateEventTime:: UTCTime -> UTCTime -> NominalDiffTime -> UTCTime
updateEventTime now time nextCanonStartTime = addUTCTime (diffUTCTime (addUTCTime nextCanonStartTime time) now) now 


-- test functions

--eventTest = [Event {time = UTCTime 2020-01-02 00:00:00, lengthEvent = 0.5, pitch = 60.0, instrument = "saw", amp = 0.16666666666666666},Event {time = UTCTime 2020-01-02 00:00:00.5, lengthEvent = 0.5, pitch = 62.0, instrument = "saw", amp = 0.16666666666666666}]

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