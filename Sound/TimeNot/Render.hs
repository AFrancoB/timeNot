{-# LANGUAGE OverloadedStrings #-}
module Sound.TimeNot.Render
(testTimes,
testPitch,
testInstr,
canonToEvents,
repeater,
eucToOnsetPattern,
fullEucToOnsetPattern,
{- toCollider, -}
mapForEstuary,
render,
onsetToOnset
) where

import Data.List
import Data.Ord (comparing)
import Data.Time
import Music.Theory.Bjorklund
import Data.Fixed
import qualified Data.Map.Strict as Mp
import qualified Data.Text as Tx
import Data.Text (Text)
import qualified Sound.OSC as Osc

import Sound.TimeNot.AST

mapForEstuary:: Event -> (UTCTime, Mp.Map Text Osc.Datum)
mapForEstuary (Event ti sus pi inst amp n pan spd nt shp ctff) =
    let tiempo = ti
        mapa = Mp.fromList [("end", Osc.double sus), ("note", Osc.double nt), ("speed", Osc.double spd), ("sample_name", Osc.string inst), ("sample_n", Osc.int32 n), ("gain", Osc.double amp), ("pan", Osc.double pan)]
    in (tiempo, mapa)

-- common source estuary type tempo
    --  (CPS, at, beat) (work in this)           [Event]     UTCTime -> UTCTime -> [Event]
render :: UTCTime -> Program -> EventF
render oTime p windowStart windowEnd = concat $ fmap (\exp -> expToEvent oTime exp windowStart windowEnd) p

expToEvent :: UTCTime -> Expression -> EventF
expToEvent oTime (RunTempo t) windowStart windowEnd = []
expToEvent oTime (RunCanon exp) windowStart windowEnd = sLPToEventF (canonToEvents oTime exp) windowStart windowEnd

testCanToEv:: Canon
testCanToEv = Canon {clength = ([2.0],True), onsetPattern = Onsets [(True,False),(True,False),(False,False),(True,False)], voices = [(1.0,0.0),(2.0,12.0),(3.0,-12.0)], canonType = Convergence (CP 4), streams = Synth (Waveshape ["saw","tri"]) "iso" [60.0, 62.0, 63.0, 65.0] ([0.5],[0.5]) ([0],[0]) ([0.5],[0]) ([1.0],[0.0]) ([0.0],[0.0])}
testCanDirtToEv = Canon {clength = ([2.0],True), onsetPattern = Onsets [(True,False),(True,False),(False,False),(True,False)], voices = [(1.0,0.0),(2.0,12.0),(3.0,-12.0)], canonType = Convergence (CP 4), streams = Dirt (Dirties [("bd", 0)]) "iso" [60.0, 62.0, 63.0, 65.0] ([0.5],[0.5]) n' ([0.5],[0]) ([1.0],[0.0]) ([0.0],[0.0])}
-- |. 2s | 4s | 3s .|

-- test
makeTime = UTCTime (fromGregorian 2020 01 10) . fromIntegral

wStart = makeTime 0

wEnd = makeTime 6

oTime = makeTime 0

period = 2 :: NominalDiffTime
(canon', otime, period') = (canonToEvents oTime testCanToEv)
times = map ptime canon'
result = map (\n -> map time $ sLPToEventF (canon', otime, period') (makeTime n) (makeTime (n + 1))) [0..10]

pureToEvent:: UTCTime -> PureEvent -> Event
pureToEvent baseUTCTime e  = 
    let t = (ptime e)
        nomTime = realToFrac t :: NominalDiffTime
        tiempo = addUTCTime nomTime baseUTCTime
    in Event {time = tiempo, 
              lengthEvent = (plengthEvent e), 
              pitch = (ppitch e), 
              instrument = (pinstrument e), 
              amp = (pamp e),
              n = (pn e),
              pan = (ppan e),
              speed = (pspeed e),
              note = (pnote e),
              shape = (pshape e),
              cutOff = (pcutOff e)}

sLPToEventF:: SLPPure -> EventF -- this is the last step to work ON!!!!!
sLPToEventF (es,oTime,period) wS wE = 
        let wsperc = realToFrac (diffUTCTime wS oTime) :: Double
            wLength = realToFrac (diffUTCTime wE wS) :: Double
            period' = realToFrac period :: Double
            startOfWindow = mod' wsperc period' 
            endOfWindow = startOfWindow + wLength
            infes = infinitizeEvents period' es
            window = filter ((>=startOfWindow) . ptime) $ takeWhile ((<endOfWindow) . ptime) infes
            normalizedWindow = map (\e-> pureToEvent wS $ e {ptime = (ptime e) - startOfWindow}) window :: [Event]
        in  normalizedWindow

infinitizeEvents:: Double -> [PureEvent] -> [PureEvent]
infinitizeEvents period events = concat $ map (\cycle -> (map (\e -> e {ptime = (period*cycle)+(ptime e)}) events)) [0..]

eventsToSLP:: [PureEvent] -> UTCTime -> [CanonDuration] -> SLPPure
eventsToSLP es oTime canDurs = (es, oTime, realToFrac (sum canDurs) :: NominalDiffTime)
                          
canonToEvents :: UTCTime -> Canon -> SLPPure
canonToEvents oTime x = 
    let period = (clength x)
        scaling = scalingFactor (onsetPattern x) (voices x) (canonType x) (period) :: [Time]
        times = canonicTime (onsetPattern x) (voices x) (canonType x) :: [[Time]]
        totalDur = (sum (head times)) * (head scaling)
        evLength = eventLength (onsetPattern x) (voices x) (canonType x) -- [LeEvents]
        scaledTimes = scalingToDurations (concat times) scaling -- [Times]
        scaledLength = scalingToDurations (concat evLength) scaling -- [Times]
        offsets = getOffsetCLength (period) times -- [Double]
        pitches = concat $ canonicPitch (onsetPattern x) (voices x) (streams x)
        instruments = concat $ canonicTimbre (onsetPattern x) (voices x) (streams x)
        ns = concat $ canonicN (onsetPattern x) (voices x) (streams x)
        amps = concat $ canonicParam (onsetPattern x) (voices x) "amp" (streams x)
        pans = concat $ canonicParam (onsetPattern x) (voices x) "pan" (streams x)
        notes = concat $ canonicParam (onsetPattern x) (voices x) "note" (streams x)
        speeds = concat $ canonicParam (onsetPattern x) (voices x) "speed" (streams x)
        timesOut = zipWith (+) (concat scaledTimes) offsets
        evLengthOut = concat $ scaledLength

        cyclePitch = take (length $ concat scaledTimes) $ cycle pitches
        cycleInstr = take (length $ concat scaledTimes) $ cycle instruments
        cycleAmps = take (length $ concat scaledTimes) $ cycle amps
        cycleNs = take (length $ concat scaledTimes) $ cycle ns
        cyclePans = take (length $ concat scaledTimes) $ cycle pans
        cycleSpeed = take (length $ concat $ scaledTimes) $ cycle speeds
        cycleNote = take (length $ concat $ scaledTimes) $ cycle notes
        zipped = zipEvents PEvent (timesOut) (evLengthOut) (cyclePitch) (cycleInstr) (cycleAmps) (cycleNs) (cyclePans) (cycleSpeed) (cycleNote) ([0]) ([0])
        events = sortBy (\ a b -> compare (ptime a) (ptime b)) zipped
    in  eventsToSLP events oTime (fst (period))  

    -- scale
scalingToDurations:: Times -> Times -> [Times]
scalingToDurations times scales = map (\x -> oneScaleAllTimes times x) scales

oneScaleAllTimes:: Times -> Time -> Times
oneScaleAllTimes times scale = map (*scale) times


getOffsetCLength:: ([CanonDuration],Loop) -> [Times] -> [Double]
getOffsetCLength canonSeqDurs cTimes =
    let seqDurs = (init $ fst canonSeqDurs)
        accSeqDurs = scanl (+) 0 seqDurs
        events = length $ head cTimes
        offsetNumber = (length cTimes)*events
    in replicateOffset accSeqDurs offsetNumber

replicateOffset:: [Double] -> Int -> [Double]
replicateOffset  accOfsets cycleLength = concat $ map (\x -> replicate cycleLength x) accOfsets

wrappAcc:: ([CanonDuration],Loop) -> ([Double], Double)
wrappAcc x = accumulateScaledTotalDurs $ fst x

-- function with a fold to create the accumulative offsets and then replicate it at the length of the canon
accumulateScaledTotalDurs:: CanonDurations -> ([Double], Double) -- this double is the total duration of the canon sequence
accumulateScaledTotalDurs scaledDurs =
    let acc = scanl (+) 0 scaledDurs
    in (init acc, last acc)

-- this are the offsets that should be added
scaledTotalDurs:: [Times] -> [LeEvents] -> CanonDurations
scaledTotalDurs cTimes cLengths = map (\x -> scaledTotalDur (fst x) (snd x)) $ zip cTimes cLengths

scaledTotalDur:: Times -> LeEvents -> CanonDuration
scaledTotalDur times lengths = (last times) + (last lengths)

----- tests ----------------
testScaledDurs:: CanonDurations
testScaledDurs = scaledTotalDurs [[0.0,1.0,2.0],[0.0,2.0,4.0]] [[1.0,1.0,1.0],[2.0,2.0,2.0]]

testTimes :: Canon -> [Time]
testTimes x = concat $ canonicTime (onsetPattern x) (voices x) (canonType x)

testPitch :: Canon -> [Pitch]
testPitch x = concat $ canonicPitch (onsetPattern x) (voices x) (streams x)

testInstr x = concat $ canonicTimbre (onsetPattern x) (voices x) (streams x)


-- adds the last onset with the last to get Total Dur of a voice
totalDur:: OnsetPattern -> VoicesData -> CanonType -> Time
totalDur onsetP voices canonType =
    let times = canonicTime onsetP voices canonType -- here is the problem!!!!
        lengths = eventLength onsetP voices canonType
        lastOnset = lastGrid onsetP voices canonType
        prop = last $ head lengths
--        prop = head $ map (fst) voices
    in lastOnset + prop

-- necesito una funcion que me devuelva 
-- el último punto de la cuadricula (ya sea onset o offset)   
testGrid = lastGrid (Onsets [(True,False),(True,False), (False, False), (False, False), (False, False), (False, False), (False, False), (False, False)]) [(1,0),(2,0)] (Convergence (CP 1))

lastGrid:: OnsetPattern -> VoicesData -> CanonType -> Double
lastGrid onsetP voices (Convergence cnvPoint) = 
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        zippAbstDur = map (snd) $ zip onsets [0.0,1.0..]
        props = map (fst) voices
        proportionals = proportions zippAbstDur props
        orderedDurs = reverse $ timeSort proportionals
        cp = funcForConvPoint cnvPoint onsets
        modCP = (cp-1) `mod` (length onsets) 
        longestBCP = head orderedDurs !! modCP  
        bcps = map (!! modCP) orderedDurs
        canonicOffsets = map (\x -> longestBCP - x) bcps
        dursAndCoffsets = zip canonicOffsets orderedDurs
        canonDurs = map (timesToCanTimes) dursAndCoffsets
        filtering = map (\x -> zip onsets x) canonDurs 
        last' = snd $ last $ head filtering
    in last'
    
totalDur':: (Times,LeEvents) -> NominalDiffTime
totalDur' (times,evDur) =
    let lastOnset = last times
        eventDur = head $ evDur
    in realToFrac (lastOnset + eventDur) :: NominalDiffTime

-- gets a number that, multiplied by each duratiov value, will scale
--all times and event Lengths to feet the canonic length
scalingFactor:: OnsetPattern -> VoicesData -> CanonType -> (CanonDurations,Loop) -> [Time] -- outputs scalingFactor, a time that multiplied by Time produces a scaled time according to one item in [cLength]
scalingFactor onsetP voices canonType clengths =
    let times = canonicTime onsetP voices canonType
        totalDuration = totalDur onsetP voices canonType
        scalingFactors = map (\x -> x / totalDuration) (fst clengths)
    in scalingFactors

-----------------------------------------------------------------

onsetToOnset :: OnsetPattern -> [Onset]
onsetToOnset (Onsets x) = x

--Example
--Canon {clength = [1.0], voices = [(4.0,0.0),(5.0,12.0),(6.0,24.0)], onsetPattern = Onsets [True,False,True,False], metricDepth = 0.5, canonType = Convergence 3, streams = Streams (Waveshape ["sin"] [60.0,67.0] "iso") [("amp",[0.5])]})
--Canon {clength = [1.0], onsetPattern = Onsets [(True,False),(True,False),(False,False),(True,False)], voices = [], canonType = Convergence 0, streams = Synth (Samples ["bd"]) "iso" [] [0.1]}

canonicTime:: OnsetPattern -> VoicesData -> CanonType -> [Times]
canonicTime onsetP voices (Convergence cnvPoint) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        cpOnset = map (snd) onsetes -- this will be used for something else!! not cp
        -- adding an "abstract" duration to each point of the onset grid to measure them so calculating the canon is possible
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

    -- You will have to add a length arbitrarily!!

funcForConvPoint:: CP -> [Bool] -> Int
funcForConvPoint (CP x) onsets = x
funcForConvPoint (CPString "last") onsets = length onsets
funcForConvPoint (CPString "lastx") onsets =
    let indexed = zip onsets [0,1..]
        filtered = filter (\par -> (fst par) == True) indexed
    in snd $ last filtered
funcForConvPoint (CPString "eje") onsets =
    let int = fromIntegral (length onsets)
        half = int/2
    in round half

-- filter out the false onsets and keeps only the Durs so Length can be added
filterOnsets:: [(Bool, Time)] -> [Time]
filterOnsets [] = []
filterOnsets x =
    let filtered =  filter (\par -> (fst par) == True) x
    in map (snd) filtered

    -- maps the durations of each onset point of the pattern to the proportions declared in voices.
proportions:: [Double] -> [Prop] -> [[Prop]]
proportions onsets [] = []
proportions onsets (p:rops) = map (\x -> (1/p)*x) onsets : proportions onsets rops

timeSort:: [[Double]] -> [[Double]]
timeSort = sortBy (comparing sum)

timesToCanTimes:: (Double, [Double]) -> [Double]
timesToCanTimes (offset, durs) = map (\x -> x +  offset) durs

-------------------------------------
-- eventLength
eventLength:: OnsetPattern -> VoicesData -> CanonType -> [LeEvents] --[[Double]]
eventLength onsetP voices canonType =
    let times = canonicTime onsetP voices canonType
        props = map (fst) voices
        rProps = map (\x -> 1/x) props
        zipped = zip rProps times
        lens = lengths onsetP -- this produces a length without grid, not implemented yet, consider it!!!
        toLength = propLens lens voices -- makes the proportional lengths without grid, not implemented yet!!
        toLengthGrid = map (\x-> propLensGrid (fst x) (snd x)) zipped
    in toLengthGrid


   -- this is the length implemented, it produces proportional event duration that takes into considerationthe grid and its ofsets
propLensGrid:: Prop -> [Time] -> [LeEvent]
propLensGrid prop times = (replicate (length times)) prop

-- This algorithm produces rpoportional length durations that only take into consideration onsets without the grid
propLens:: LeEvents -> VoicesData -> [LeEvents] -- [[Double]]
propLens lens voices = map (\x -> propLen x lens) (map (fst) voices)

propLen:: Double -> LeEvents -> LeEvents
propLen proportion events = map (*proportion) events

lengths:: OnsetPattern -> LeEvents
lengths op =
    let onsets = map (fst) $ onsetToOnset op
        indexed = zip onsets [0,1..]
        filtered = filter (\par -> (fst par) == True) indexed
    in lengthsCalculate filtered indexed

lengthsCalculate:: [(Bool, Int)] -> [(Bool,Int)] -> LeEvents
lengthsCalculate [x] events = [lastLeEvent events]
lengthsCalculate filtered indexed =
    let result = fromIntegral ((snd $ head $ tail filtered) - (snd $ head filtered)) :: LeEvent
    in result : lengthsCalculate (tail filtered) indexed


lastLeEvent:: [(Bool, Int)] -> LeEvent
lastLeEvent indexedEvents =
    let filtered = filter (\par -> (fst par) == True) indexedEvents
        lastEvent = snd $ last filtered
        lastInterval = (length indexedEvents) - lastEvent
        firstInterval = snd $ head filtered
    in fromIntegral (lastInterval + firstInterval) :: LeEvent


--------------------------------------

canonicPitch:: OnsetPattern -> VoicesData -> Streams -> [Pitches]
canonicPitch onsetP voices (Synth (Waveshape instNames) pattern pitches amps ns pans speeds notes) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        pitchStruct = paramStructure onsets pitches pattern
        -- transpositions!
        transpPitches = pitchTransps pitchStruct voices
    in transpPitches

canonicPitch onsetP voices (Sample (Samples instNames) pattern rates amps ns pans speeds notes) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        rateStruct = paramStructure onsets rates pattern
        -- transpositions!
        transpRates = rateTransps rateStruct voices
    in transpRates

canonicPitch onsetP voices (Dirt (Dirties instNames) pattern rates amps ns pans sp nt) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        rateStruct = paramStructure onsets rates pattern
        -- transpositions!
        transpRates = rateTransps rateStruct voices
    in transpRates

rateTransps:: [Rate] -> VoicesData -> [Rates] -- rateTransps does nothing but replicate the pattern of rates to fit the number of voices
rateTransps x voices =
    let transps = map (snd) voices
    in replicate (length transps) x

pitchTransps:: [Pitch] -> VoicesData -> [Pitches]
pitchTransps x voices =
    let transps = map (snd) voices
    in transport transps x

transport:: [Pitch] -> [Pitch] -> [Pitches]
transport [] _ = []
transport (tr:ansp) pitches = (map (+tr) pitches) : transport ansp pitches

-- outputs an already filtered out grid version of pitch
-- The values here are already the same as Onsets in the canonicDurationOutput

------------------------------
-- N implementation
n' = ([1,2,1,1],[5,6,7]) -- [6,7,6,6] [7,8,7,7] [8,9,8,8]
xoTest = [(True,False),(True,False),(True,False),(True,False),(True,False),(True,False),(False,False),(True,False),(True,False)]
nTest= canonicN (Onsets xoTest) [(4.0,0.0),(5.0,12.0),(6.0,24.0),(7.0,-12.0),(8.0,-24.0)] (Dirt (Dirties [("bd", 0),("feel",1)]) "iso" [] ([0.1],[0.1]) n' ([0.5],[0]) ([1.0],[0]) ([0],[0])) 

canonicN:: OnsetPattern -> VoicesData -> Streams -> [[NumSample]]
canonicN onsetP voices (Dirt (Dirties webDirts) pattern rate amps ns pans speeds notes) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        rows = fst ns
        columns' = snd ns
        columns = 
            if (length columns') > (length voices) 
                then take (length voices) columns' 
                else take (length voices) $ cycle columns' 
        matrix = map (\c -> getRows rows c) columns
        applyPattern = map (\mx-> paramStructureInteger onsets mx pattern) matrix

        voicesL = length voices
        getIndexes = map (snd) webDirts
        filtered = length $ filter (\x -> x == True) onsets
        cycled = indexCycle voicesL getIndexes
        replicates = map (\x -> replicateIndx x filtered) cycled
        zipped = recursZips replicates applyPattern
    in zipped

canonicN onsetP voices (Sample (Samples instNames) pattern rate amps ns pans sp nt) = [[0]]

canonicN onsetP voices (Synth (Waveshape instNames) pattern rate amps ns pans sp nt) = [[0]]

getRows:: [Integer] -> Integer -> [Double]
getRows rows column =  
    let r = map (+ column) rows
    in map (fromIntegral) r

recursZips:: [[Integer]] -> [[Integer]] -> [[Integer]]
recursZips [] [] = []
recursZips (x:xs) (y:ys) = zipWith (+) x y : recursZips xs ys  

------------------------------------
-- amp implementation
ampTest= canonicAmp (Onsets xoTest) [(4.0,0.0),(5.0,12.0),(6.0,24.0),(7.0,-12.0),(8.0,-24.0)] (Dirt (Dirties [("bd", 0),("feel",1)]) "iso" [] ([0.1],[0.1]) n' ([0.5],[0]) ([1],[0]) ([0],[0])) 

canonicAmp:: OnsetPattern -> VoicesData -> Streams -> [[Amp]]
canonicAmp onsetP voices (Synth x pattern pitches amps ns pans sp nt) = canonicAmp' onsetP voices amps pattern
canonicAmp onsetP voices (Sample x pattern rates amps ns pans sp nt) = canonicAmp' onsetP voices amps pattern
canonicAmp onsetP voices (Dirt x pattern rates amps ns pans sp nt) = canonicAmp' onsetP voices amps pattern

canonicAmp':: OnsetPattern -> VoicesData -> Amps -> StreamPattern -> [[Amp]]
canonicAmp' onsetP voices amps pattern =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        rows = fst amps
        columns' = snd amps
        columns = 
            if (length columns') > (length voices) 
                then take (length voices) columns' 
                else take (length voices) $ cycle columns' 
        matrix = map (\c -> getRowsD rows c) columns
        applyPattern = map (\mx-> paramStructure onsets mx pattern) matrix
    in applyPattern

getRowsD:: [Double] -> Double -> [Double]
getRowsD rows column =  map (+ column) rows

------------------------------------
-- pan implementation
panTest= canonicPan (Onsets xoTest) [(4.0,0.0),(5.0,12.0),(6.0,24.0),(7.0,-12.0),(8.0,-24.0)] (Dirt (Dirties [("bd", 0),("feel",1)]) "iso" [] ([0.1],[0.1]) n' ([0.5],[0]) ([1],[0]) ([0],[0])) 

canonicPan:: OnsetPattern -> VoicesData -> Streams -> [[Pan]]
canonicPan onsetP voices (Synth x pattern pitches amps ns pans sp nt) = canonicPan' onsetP voices pans pattern
canonicPan onsetP voices (Sample x pattern rates amps ns pans sp nt) = canonicPan' onsetP voices pans pattern
canonicPan onsetP voices (Dirt x pattern rates amps ns pans sp nt) = canonicPan' onsetP voices pans pattern

canonicPan':: OnsetPattern -> VoicesData -> Pans -> StreamPattern -> [[Pan]]
canonicPan' onsetP voices pans pattern =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        rows = fst pans
        columns' = snd pans
        columns = 
            if (length columns') > (length voices) 
                then take (length voices) columns' 
                else take (length voices) $ cycle columns' 
        matrix = map (\c -> getRowsD rows c) columns
        applyPattern = map (\mx-> paramStructure onsets mx pattern) matrix
    in applyPattern

------------------------------------
-- pan implementation
speedTest= canonicPan (Onsets xoTest) [(4.0,0.0),(5.0,12.0),(6.0,24.0),(7.0,-12.0),(8.0,-24.0)] (Dirt (Dirties [("bd", 0),("feel",1)]) "iso" [] ([0.1],[0.1]) n' ([0.5],[0]) ([1],[0]) ([0],[0])) 

canonicParam:: OnsetPattern -> VoicesData -> String -> Streams -> [[Pan]]
canonicParam onsetP voices which (Synth x pattern pitches amps ns pans spd nt) 
    | (which == "amp") = canonicParam' onsetP voices amps pattern
    | (which == "pan") = canonicParam' onsetP voices pans pattern
    | (which == "spd") = canonicParam' onsetP voices spd pattern
    | (which == "note") = canonicParam' onsetP voices nt pattern
canonicParam onsetP voices which (Sample x pattern rates amps ns pans spd nt) 
    | (which == "amp") = canonicParam' onsetP voices amps pattern
    | (which == "pan") = canonicParam' onsetP voices pans pattern
    | (which == "spd") = canonicParam' onsetP voices spd pattern
    | (which == "note") = canonicParam' onsetP voices nt pattern
canonicParam onsetP voices which (Dirt x pattern rates amps ns pans spd nt)
    | (which == "amp") = canonicParam' onsetP voices amps pattern
    | (which == "pan") = canonicParam' onsetP voices pans pattern
    | (which == "spd") = canonicParam' onsetP voices spd pattern
    | (which == "note") = canonicParam' onsetP voices nt pattern 

canonicParam':: OnsetPattern -> VoicesData -> ([Double],[Double]) -> StreamPattern -> [[Double]]
canonicParam' onsetP voices param pattern =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        rows = fst param
        columns' = snd param
        columns = 
            if (length columns') > (length voices) 
                then take (length voices) columns' 
                else take (length voices) $ cycle columns' 
        matrix = map (\c -> getRowsD rows c) columns
        applyPattern = map (\mx-> paramStructure onsets mx pattern) matrix
    in applyPattern

-------------------------------------------------------

paramStructureInteger:: [Bool] -> [Double] -> StreamPattern -> [Integer] 
paramStructureInteger onsets params pattern = map (floor) $ paramStructure onsets params pattern

paramStructure:: [Bool] -> [Double] -> StreamPattern -> [Double]
paramStructure onsets params "isoGrid" = paramToIsoGrid onsets params
paramStructure onsets params "iso" = paramToIso onsets params
paramStructure onsets params "eu" = paramToEu onsets params
paramStructure onsets params _ = error "not mapped yet"

-- Isos to Structure
paramToIsoGrid:: [Bool] -> [Double] -> [Double]
paramToIsoGrid onsets params =
    let paramWithOnsets = take (length onsets) (cycle params)
        zipped = zip onsets paramWithOnsets
        filtered = filter (\x -> fst x == True ) zipped
    in map (snd) filtered

paramToIso:: [Bool] -> [Double] -> [Double]
paramToIso onsets params =
    let filtered = filter (\x -> x == True) onsets
        cyclingParam = take (length filtered) (cycle params)
        zipped = zip cyclingParam params
    in cyclingParam

-- Eucledian Structure
paramToEu:: [Bool] -> [Double] -> [Double]
paramToEu onsets params =
    let bjork = bjorklund'' onsets params
        concats = concat (guardBjorkRepeat params bjork)
        zipped = zip onsets (concats)
        filtered = filter (\x -> fst x == True) zipped
    in map (snd) filtered

bjorkRepeat:: [Double] -> [Int] -> [[Double]]
bjorkRepeat [] [] = []
bjorkRepeat (param:params) (bjork:bjorks) = replicate bjork param : bjorkRepeat params bjorks

guardBjorkRepeat:: [Double] -> [Int] -> [[Double]]
guardBjorkRepeat params bjorks
    | (length params) > (length bjorks) = bjorkRepeat params (take (length params) $ cycle bjorks)
    | (length params) < (length bjorks) = bjorkRepeat (take (length bjorks) $ cycle params) bjorks
    | otherwise = bjorkRepeat params bjorks



bjorklund'':: [Bool] -> [Double] -> [Int]
bjorklund'' onsets params =
    let bjork = bjorklund ((length params), (length onsets))
        bjork2 = zip bjork [0,1..]
        bjork3 = map (snd) (filter (\x -> fst x == True) bjork2)
        bjork4 = bjork3 ++ [length bjork]
    in listToBjork'' bjork4

-- creates an array of lengths or number of repetitions for each pitch or timbre name
listToBjork'':: [Int] -> [Int]
listToBjork'' [] = []
listToBjork'' [x] = [x]
listToBjork'' [x,y] = [y - x]
listToBjork'' (x:y:ys) = y - x : listToBjork'' (y:ys)


------------------------------------------------------------------
-- implement the instrument!!

canonicTimbre:: OnsetPattern -> VoicesData -> Streams -> [InstNames]
canonicTimbre onsetP voices (Sample (Samples instNames) pattern rate amp ns pans sp nt) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        voicesL = length voices
            -- filters out the false onsetpoints
        filtered = length $ filter (\x -> x == True) onsets
            -- creates the instruments per voice list
        cycled = instrCycle voicesL instNames
    in map (\x -> replicateInst x filtered) cycled

canonicTimbre onsetP voices (Dirt (Dirties webDirts) pattern rate amp ns pans sp nt) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        voicesL = length voices
        getInstrs = map (fst) webDirts
            -- filters out the false onsetpoints
        filtered = length $ filter (\x -> x == True) onsets
            -- creates the instruments per voice list
        cycled = instrCycle voicesL getInstrs
    in map (\x -> replicateInst x filtered) cycled

canonicTimbre onsetP voices (Synth (Waveshape instNames) pattern pitch amp ns pans sp nt) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        voicesL = length voices
            -- filters out the false onsetpoints
        filtered = length $ filter (\x -> x == True) onsets
            -- creates the instruments per voice list
        cycled = instrCycle voicesL instNames
    in map (\x -> replicateInst x filtered) cycled

------ get N of samples

replicateInst:: String -> Int -> [String]
replicateInst instr onsets = replicate onsets instr

replicateIndx:: Index -> Int -> [Index]
replicateIndx index onsets = replicate onsets index

instrCycle:: Int -> InstNames -> InstNames
instrCycle voices timbres = take voices $ cycle timbres

indexCycle:: Int -> [Index] -> [Index]
indexCycle voices timbres = take voices $ cycle timbres

------------------------------------- To parser Funcs------------------------------

eucToOnsetPattern :: Int -> Int -> Int -> Int -> OnsetPattern -> [Onset]
eucToOnsetPattern cp k n r (Onsets onset) =
    let bjork = bjorklund (k,n)
        patt = map (\x -> subPattToEucPatt x onset) bjork
        rot = rotate r $ concat patt
    in rot
-- OJO!!!!! Aquí es necesario implementar una función para que solo se pase la última instancia
-- del CP en la repetición del patrón

subPattToEucPatt :: Bool -> [Onset] -> [Onset]
subPattToEucPatt bool onset = if bool==True then onset else (replicate (length onset) (False,False))

----- Full Euclidean

fullEucToOnsetPattern :: (Int,OnsetPattern) -> (Int,OnsetPattern) -> Int -> [Onset]
fullEucToOnsetPattern (k,(Onsets onsetX)) (n,(Onsets onsetO)) r =
    let bjork = bjorklund (k,n) 
        patt = map (\x -> if x == True then onsetX else onsetO) bjork
        rot = rotate r $ concat patt
    in rot
-- OJO!!!!! Aquí es necesario implementar una función para que solo se pase la última instancia
-- del CP en la repetición del patrón

repeater:: [Onset] -> Int -> [Onset]
repeater val num =
    let x = replicate num val
    in concat x

rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs



getNextWindow startTime endTime restOfCanon =
  let windowLength = length $ takeWhile ((endTime >) .  time ) restOfCanon
  in splitAt windowLength restOfCanon


zipEvents:: (a->b->c->d->e->f->g->h->i->j->k->l) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]->[k]->[l]
zipEvents func (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) (j:js) (k:ks)
                   =  func a b c d e f g h i j k : zipEvents func as bs cs ds es fs gs hs is js ks
zipEvents _ _ _ _ _ _ _ _ _ _ _ _ = []

