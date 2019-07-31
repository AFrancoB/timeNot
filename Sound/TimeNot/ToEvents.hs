module Sound.TimeNot.ToEvents
(testTimes,
testPitch,
testInstr,
canonToEvents,
repeater,
eucToOnsetPattern,
{- toCollider, -}
progToEvents,
onsetToOnset,
toDB
) where

import Data.List
import Data.Ord (comparing)
import Data.Time
import Music.Theory.Bjorklund

import Sound.TimeNot.AST


-- toCollider:: UTCTime -> Program -> IO()
-- toCollider now prog = sendEvents $ progToEvents now prog
--toCollider prog = sequence $ fmap (sendEvents) $ progToEvents prog

progToEvents :: UTCTime -> Program -> [Event]
progToEvents now x = concat $ fmap (expToEvent now) x

expToEvent :: UTCTime -> Expression -> [Event]
expToEvent now (RunTempo t) = []
expToEvent now (RunCanon c) = canonToEvents now c

testCanToEv:: Canon
testCanToEv = Canon {clength = ([2.0,4.0,3.0],True), onsetPattern = Onsets [(True,False),(True,False),(False,False),(True,False)], voices = [(1.0,0.0),(2.0,12.0),(3.0,-12.0)], canonType = Convergence (CP 1), streams = Synth (Waveshape ["saw","tri"]) "iso" [60.0, 62.0, 63.0, 65.0] [0.5]}
-- |. 2s | 4s | 3s .|

canonToEvents :: UTCTime -> Canon -> [Event]
canonToEvents now x =
    let timesOut = zipWith (+) (concat scaledTimes) offsets
        nomTime = doubleToNDT timesOut
        timesOut' = fmap (flip addUTCTime now) nomTime
        evLengthOut = concat $ scaledLength
        cyclePitch = take (length $ concat scaledTimes) $ cycle pitches
        cycleInstr = take (length $ concat scaledTimes) $ cycle instruments
        cycleAmps = take (length $ concat scaledTimes) $ cycle amps
        zipped = zipWith5 Event (timesOut') (evLengthOut) (cyclePitch) (cycleInstr) (cycleAmps)
    in zipped
  where
    scaling = scalingFactor (onsetPattern x) (voices x) (canonType x) (clength x) -- [Time]
    times = canonicTime (onsetPattern x) (voices x) (canonType x)  -- [Times]
    evLength = eventLength (onsetPattern x) (voices x) (canonType x) -- [LeEvents]
    scaledTimes = scalingToDurations (concat times) scaling -- [Times]
    scaledLength = scalingToDurations (concat evLength) scaling -- [Times]
 --   accOffsets = wrappAcc (clength x) -- ([Double],[Double])
    offsets = getOffsetCLength (clength x) times -- [Double]
    pitches = concat $ canonicPitch (onsetPattern x) (voices x) (streams x)
    instruments = concat $ canonicTimbre (onsetPattern x) (voices x) (streams x)
    amps = concat $ canonicAmp (onsetPattern x) (voices x) (streams x)


doubleToNDT:: [Time] -> [NominalDiffTime]
doubleToNDT t = map (\x -> realToFrac x :: NominalDiffTime) t


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


-- ofsetsToCTimes::
-- getOffsetCLength:: ([Double], Double) -> [Times] -> [Double] -- number of voices
-- getOffsetCLength accDurs voices =
--     let events = length $ head voices
--         offsetNumber = (length voices)*events
--     in replicateOffset accDurs offsetNumber


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
    let times = canonicTime onsetP voices canonType
        lengths = eventLength onsetP voices canonType
        lastOnset = last $ head times
        prop = last $ head lengths
--        prop = head $ map (fst) voices
    in lastOnset + prop

totalDur':: (Times,LeEvents) -> Time
totalDur' (times,evDur) =
    let lastOnset = last times
        eventDur = head $ evDur
    in lastOnset + eventDur

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
canonicPitch onsetP voices (Synth (Waveshape instNames) pattern pitches amps) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        pitchStruct = paramStructure onsets pitches pattern
        -- transpositions!
        transpPitches = pitchTransps pitchStruct voices
    in transpPitches

canonicPitch onsetP voices (Sample (Samples instNames) pattern rates amps) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        rateStruct = paramStructure onsets rates pattern
        -- transpositions!
        transpRates = rateTransps rateStruct voices
    in transpRates

canonicPitch onsetP voices (Dirt (Dirties instNames) pattern rates amps) =
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
------------------------------------
-- amp implementation

canonicAmp:: OnsetPattern -> VoicesData -> Streams -> [Amps]
canonicAmp onsetP voices (Synth (Waveshape instNames) pattern pitches amps) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        ampStruct = paramStructure onsets amps pattern
        -- transpositions!
        transpAmps = map (\x -> x / (fromIntegral (length (voices)) :: Double )) ampStruct
        voicesAmps = replicate (length voices) transpAmps
    in voicesAmps

canonicAmp onsetP voices (Sample (Samples instNames) pattern rate amps) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        ampStruct = paramStructure onsets amps pattern
        -- transpositions!
        transpAmps = map (\x -> x / (fromIntegral (length (voices)) :: Double )) ampStruct
        voicesAmps = replicate (length voices) transpAmps
    in voicesAmps

canonicAmp onsetP voices (Dirt (Dirties instNames) pattern rate amps) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        -- structure the pitch series in a structure depending in the organisation pattern:
        ampStruct = paramStructure onsets amps pattern
        -- transpositions!
        transpAmps = map (\x -> x / (fromIntegral (length (voices)) :: Double )) ampStruct
        voicesAmps = replicate (length voices) transpAmps
    in voicesAmps

-------------------------------------------------------
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
canonicTimbre onsetP voices (Sample (Samples instNames) pattern rate amp) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        voicesL = length voices
            -- filters out the false onsetpoints
        filtered = length $ filter (\x -> x == True) onsets
            -- creates the instruments per voice list
        cycled = instrCycle voicesL instNames
    in map (\x -> replicateInst x filtered) cycled

canonicTimbre onsetP voices (Dirt (Dirties instNames) pattern rate amp) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        voicesL = length voices
            -- filters out the false onsetpoints
        filtered = length $ filter (\x -> x == True) onsets
            -- creates the instruments per voice list
        cycled = instrCycle voicesL instNames
    in map (\x -> replicateInst x filtered) cycled

canonicTimbre onsetP voices (Synth (Waveshape instNames) pattern pitch amp) =
    let onsetes = onsetToOnset onsetP
        onsets = map (fst) onsetes
        voicesL = length voices
            -- filters out the false onsetpoints
        filtered = length $ filter (\x -> x == True) onsets
            -- creates the instruments per voice list
        cycled = instrCycle voicesL instNames
    in map (\x -> replicateInst x filtered) cycled

replicateInst:: String -> Int -> [String]
replicateInst instr onsets = replicate onsets instr

instrCycle:: Int -> InstNames -> InstNames
instrCycle voices timbres = take voices $ cycle timbres
------------------------------------------------------------------

-- canonicParams:: OnsetPattern -> VoicesData -> Streams -> [Params]
-- canonicParams onsetP voices (Streams (Waveshape instName pitch patt) parametros) =
--     let onsets = onsetToBool onsetP
--         trueOnsets = length $ filter (\x -> x == True) onsets
--         paramNams = map (\x -> paramName x) parametros
--         paramStruct = map (\x-> paramStructure onsets (snd x) patt) parametros
--         canonise = replicate (length voices) (parStringParam paramNams paramStruct)
--     in canonise

-- paramName:: Param -> String
-- paramName param = fst param

-- parStringParam:: [String] -> [[Double]] -> [(String, [Double])]
-- parStringParam [] [] = []
-- parStringParam (x:xs) (y:ys) = (x, y) : parStringParam xs ys


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



repeater:: [Onset] -> Int -> [Onset]
repeater val num =
    let x = replicate num val
    in concat x


-- rotate :: Int -> [a] -> [a]
-- rotate _ [] = []
-- rotate n xs = zipWith const (drop n (cycle xs)) xs

rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs


toDB:: Num a => a -> a
toDB x = x*(-1)








-- to incorporate various scaling factors:

    -- let timesAndScales = map (\x -> timesWithScale times x) scaling
    -- evLengthScaled = map (\x -> timesWithScale evLength x) scaling

    -- totalDurs =map (\x-> totalDur' x) $ zip timesAndScales evLengthScaled
    -- accDurs = scanl (+) 0 totalDurs
    -- timesWithAccDurs = zip timesAndScales accDurs
    -- plusOffset = concat $ map (\x -> scaledTimePlusOffset x) timesWithAccDurs
    -- allTimes = plusOffset

    -- evLengthconcat = concat evLengthScaled
    -- pitchCycled = take (length timesAndScales) $ cycle pitches
    -- instrCycled = take (length timesAndScales) $ cycle instruments
