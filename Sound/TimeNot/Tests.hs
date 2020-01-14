module Sound.TimeNot.Tests where

import Data.List
import Data.Ord (comparing)
import Data.Time
import Music.Theory.Bjorklund

import Sound.TimeNot.AST
import Sound.TimeNot.InfinityFuncs


timeTest = canonicTime' (Onsets [(True,False),(True,False),(False,False),(True,False)]) [(1,0),(2,12),(4,-12)] (Convergence (CP 4))

tempoStart = 0.0
windowStart = 5.2
windowEnd = 10.0


-- this function outputs the time in which the adequate cycle starts for the window to see
getCycleTime:: CanonDuration -> Double -> [Double] -> Double
getCycleTime canDur window [0] = getCycleTime (canDur*1) window [1]
getCycleTime canDur window count 
    | ((last count)*canDur) > window = ((last count) -1) * canDur
    | otherwise = getCycleTime (canDur * (last count)) window (count ++ [((last count) + 1)])

func:: CanonDuration -> Double -> Double -> [Times] -> [[(Double, Index)]]
func canDur wStart wEnd canonicTimes = 
    let cycleStart = getCycleTime canDur wStart [0]
        cycleEnd = getCycleTime canDur wEnd [0]
        timeWithID = addID canonicTimes
        eventTimes = map (\x -> func1 cycleStart x) timeWithID
    in eventTimes

func1:: Double -> [(Double,Index)] -> [(Double,Index)]
func1 cycle canTime = map (\x -> ((fst x)+cycle, snd x)) canTime

-- [[0.0,1.0,3.0],[1.0,1.5,2.5],[1.5,1.75,2.25]]


--addID:: [Times] -> [[(Double,Index)]]
addID timess = 
    let numVoices = [0..(length timess -1)]
        numEvPerVoice = map (length) timess
        voiceIndex = map (\x -> replicate (head numEvPerVoice) x) numVoices
--    in voiceIndex
        tuplets = map (\times -> addIndex times) timess
    in tuplets

addIndex:: Times -> [(Double,Index)]
addIndex times = zip times [0..]

canonicTime':: OnsetPattern -> VoicesData -> CanonType -> [Times]
canonicTime' onsetP voices (Convergence cnvPoint) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        zippAbstDur = map (snd) $ zip onsets [0.0,1.0..]
        -- retrieving the proportions from the tuplet of voices
        props = map (fst) voices
        -- trasnform the abstract durations into proportional durations
        proportionals = proportions zippAbstDur props
        -- ordering each list of durations from longest to shortest to calculate the offset for the canon
        orderedDurs = reverse $ timeSort proportionals
        -- getting the cp;
        cp = funcForConvPoint cnvPoint onsets
        -- here starts the calculations for the canon
        modCP = (cp-1) `mod` (length onsets) -- -1 allows to count actual events, not the index of the events
        longestBCP = head orderedDurs !! modCP  -- Before Convergence Point (BCP) time
        bcps = map (!! modCP) orderedDurs
        canonicOffsets = map (\x -> longestBCP - x) bcps
        dursAndCoffsets = zip canonicOffsets orderedDurs
        canonDurs = map (timesToCanTimes) dursAndCoffsets
        -- filter out the durs that are not True in the Onset Pattern
        filtering = map (\x -> zip onsets x) canonDurs
        filtered = map (\x -> filterOnsets x) filtering -- here we have [Times] so far
    in filtered

timesToCanTimes:: (Double, [Double]) -> [Double]
timesToCanTimes (offset, durs) = map (\x -> x +  offset) durs


-- maps the durations of each onset point of the pattern to the proportions declared in voices.
proportions:: [Double] -> [Prop] -> [[Prop]]
proportions onsets [] = []
proportions onsets (p:rops) = map (\x -> (1/p)*x) onsets : proportions onsets rops

timeSort:: [[Double]] -> [[Double]]
timeSort = sortBy (comparing sum)

onsetToOnset :: OnsetPattern -> [Onset]
onsetToOnset (Onsets x) = x

funcForConvPoint:: CP -> [Bool] -> Int
funcForConvPoint (CP x) onsets = x
funcForConvPoint (CPString "last") onsets = length onsets
funcForConvPoint (CPString "lastx") onsets =
    let indexed = zip onsets [0,1..]
        filtered = filter (\par -> (fst par) == True) indexed
    in snd $ last filtered
funcForConvPoint (CPString "palindrome") onsets =
    let int = fromIntegral (length onsets)
        half = int/2
    in round half

-- filter out the false onsets and keeps only the Durs so Length can be added
filterOnsets:: [(Bool, Time)] -> [Time]
filterOnsets [] = []
filterOnsets x =
    let filtered =  filter (\par -> (fst par) == True) x
    in map (snd) filtered



-- cabal2nix httpsXXXXXXXX --revision commitXXXXXXX



--       startTime   endTime
-- func:: EventF

-- dur == 3

-- --             startT    endT
-- type EventF = UTCTime -> UTCTime -> [Events] 


-- sched':: mVar (EventF)