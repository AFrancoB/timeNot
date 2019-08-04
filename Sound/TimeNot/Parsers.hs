module Sound.TimeNot.Parsers where

import Text.Parsec 
import Text.ParserCombinators.Parsec.Prim hiding (try)
import Data.Functor.Identity
import qualified Text.Parsec.Token as P

-- My stuff
import Sound.TimeNot.AST
import Sound.TimeNot.ToEvents

--Parser to put together the structures! -------------------------------

-- mejorar esta funcion
--testEvaluate = evaluate "write a program"

-- this allows me to have several expressions at the same time
topLevelParser :: Parser [Expression]
topLevelParser = do
  whiteSpace
  x <- expression `sepBy` reservedOp ";"
--  optional semi
  eof
  return x

  
runCanonParser :: String -> Either ParseError [Expression]
runCanonParser x = parse topLevelParser "" x

expression :: Parser Expression
expression = choice [runTiempo,runCanon]

runCanon :: Parser Expression
runCanon = RunCanon <$> canonParser

-- this will have to change the bpms/cps when implemented in Estuary and even connected to the TempoClock of SC 
runTiempo :: Parser Expression
runTiempo = RunTempo <$> tiempoParser


tiempoParser:: Parser TiempoChange
tiempoParser = do
    x <- choice [cpsParser, bpmParser]
    return x 

cpsParser:: Parser TiempoChange
cpsParser = do
    try $ reserved "cps:"
    x <- float
    return (BPM x)

bpmParser:: Parser TiempoChange
bpmParser = do
    try $ reserved "bpm:" 
    x <- float
    return (CPS x)

-- example:
-- runCanonParser "|: 1.0 :| Temp: 4.0:5.0:6.0:7.0 Transp: 0.0|12.0|24.0 xoxom2 conv 3 sin 60.0 67.0 iso"
canonParser:: Parser Canon
canonParser = do
    length <- durationParser <|> return ([2.0], True)
    onset <- onsetPatternParser
    voiceData <- manualVoicesParser <|> return [(1.0,0.0)]
    canType <- canonTypeParser <|> return (Convergence (CP 1))
    stream <- streamParser <|> return (Synth( Waveshape ["sin"]) "eu" [48.0] [0.5])
    return (Canon length onset voiceData canType stream)

-- maybe this is better?
-- canonParser:: Parser Canon
-- canonParser = do
--     length <- lengthParser <|> return [1.0]
--     rhythm <- rhythmPatternParser -- onsetPatternParser
--     canonic <- canoniseParser -- <|> type and voicParser
--     stream <- streamParser
    
------------------------------------------
-- canonParser :: Parser Canon
-- canonParser = Canon <$> lengthParser
--     <*> manualVoicesParser 
--     <*> onsetPatternParser 
--     <*> parserMetricDepth 
--     <*> canonTypeParser 
--     <*> streamParser


-- parsing the parts of the canon data


------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
----------------duration Parser------------------------------

durationParser:: Parser ([CanonDuration], Loop)
durationParser= do
    x <- choice [loopParser, noLoopParser]
    return x

loopParser:: Parser ([CanonDuration], Loop)
loopParser= do
    try (reserved "|:") 
    durees <- (sepBy (choice [try( secondParser), try(tempoParser), cycleParser]) (char '|')) 
    try (reserved ":|")
    return (concat(durees), True)

noLoopParser:: Parser ([CanonDuration], Loop)
noLoopParser= do
    try (reserved "|.")
    durees <- (sepBy (choice [try( secondParser), try(tempoParser), cycleParser]) (char '|'))
    try (reserved ".|")
    return (concat(durees), False)

secondParser :: Parser [CanonDuration]
secondParser = do
    whiteSpace
    t <- double  <|> (return 2.0)
    reserved "s"
    md <- parserMetricDepth <|> (return 1.0)
    reps <- durRepParser <|> (return 1)
    return (replicate reps (t*md))  

tempoParser :: Parser [CanonDuration]
tempoParser = do
    whiteSpace
    x <- double  <|> (return 4.0)
    reserved "t"
    md <- parserMetricDepth <|> (return 1.0)
    reps <- durRepParser <|> (return 1)
    return (replicate reps ((bpmToSecs x (BPM 60))*md)) -- OJO! 60 is the placeholder of the tempo change, I need to figure otu how to apply tempochanges  

bpmToSecs:: Double -> TiempoChange -> CanonDuration
bpmToSecs x (BPM t) = (60/t) * x

cycleParser :: Parser [CanonDuration]
cycleParser = do
    whiteSpace
    x <- double  <|> (return 1.0)
    reserved "c"
    md <- parserMetricDepth <|> (return 1.0)
    reps <- durRepParser <|> (return 1)
    return (replicate reps ((cpsToSecs x (CPS 1))*md)) -- OJO, same case as in tempoParser  

cpsToSecs:: Double -> TiempoChange -> CanonDuration
cpsToSecs x (CPS c) =  (1/c) * x

durRepParser:: Parser Int 
durRepParser = do
    try $ reserved "%"
    x <- integer
    return (fromIntegral x :: Int)


    -- test
durParserTest :: String -> Either ParseError ([CanonDuration], Loop)
durParserTest x = parse durationParser "" x

-----------------------------------------------------------------------
-- MetricDepth parser

parserMetricDepth :: Parser MetricDepth
parserMetricDepth = do
    ((try (reserved "m2")) >> return 0.5)
    <|> ((try (reserved "m4")) >> return 0.25)
    <|> ((try (reserved "m8")) >> return 0.125)
    <|> ((try (reserved "M2")) >> return 2.0)
    <|> ((try (reserved "M4")) >> return 4.0)
    <|> ((try (reserved "M8")) >> return 8.0)
    <|> ((try (reserved "M")) >> return (1.0))
    <|> ((try (reserved "m")) >> return (1.0))
    <|> return (1.0)


pruebaMetricDepth :: String -> Either ParseError MetricDepth    
pruebaMetricDepth x = parse parserMetricDepth "" x




-- onsetPatternParser:: Parser OnsetPattern
-- onsetPatternParser = do
--     x <- many $ choice [ onsetParser, try $ euclidianParser, try $ repeatParser] -- here I need to add the euclidean parsers, no clue how!!
--     y <- repeatParser <|> return []  -- how to "go to next step" if empty? (is this the right way?)
--     z <- choice [ try( euclidianParser ), try(repeatParser), many( onsets )] <|> return []
--     whiteSpace
--     return  (Onsets $ concat [x,y,z])


onsetPatternParser:: Parser OnsetPattern
onsetPatternParser = do
    x <- many1 $ choice [ try euclidianParser , try repeatParser, try onsetParser] -- here I need to add the euclidean parsers, no clue how!!
    whiteSpace
    return (Onsets $ concat x)

onsets:: Parser Onset
onsets = do
    (oneOf ("x") >> return (True, False)) 
    <|> (oneOf ("o") >> return (False, False)) 
    <|> (oneOf ("X") >> return (True, True))
    <|> (oneOf ("O") >> return (False, True))

onsetParser:: Parser [Onset]
onsetParser = do
    x <- many1 $ onsets
    whiteSpace
    return x

repeatParser:: Parser [Onset]
repeatParser = do
    try (oneOf "!")
    val <- onsetPatternParser
    try (reserved "#")
    num <- integer 
    return (repeater (onsetToOnset val) (fromIntegral num :: Int))



-- to test the parser use this function --
pruebaOnsetPattern :: String -> Either ParseError OnsetPattern
pruebaOnsetPattern x = parse onsetPatternParser "" x

-------------------------------------------------------------
    
--  euclidian:

euclidianParser:: Parser [Onset]
euclidianParser = do
    v <- integer
    try (reservedOp ":e:")
    w <- integer
    optional (try (reservedOp ":r:")) 
    x <- integer <|> (return 0)
    y <- parserP <|> return (Onsets [(True,False)])

    return (eucToOnsetPattern (fromIntegral 0 :: Int) (fromIntegral v :: Int) (fromIntegral w :: Int) (fromIntegral x :: Int) (fromEmptyToTrue y))
-- OJO. First value of eucToOnsetPattern is a CP, an idea that never worked properly. 
-- Needs to be removed in the future.

fromEmptyToTrue:: OnsetPattern -> OnsetPattern
fromEmptyToTrue (Onsets []) = Onsets [(True,False)]
fromEmptyToTrue x = x

parserP:: Parser OnsetPattern 
parserP = do
    try $ reserved "p:"
    x <- onsetPatternParser <|> (return (Onsets [(True, False)]))
    return x

pruebaEucledian :: String -> Either ParseError [Onset]
pruebaEucledian x = parse euclidianParser "" x


---------------------------------------------------------------------
--   Canonise:    -- probably this is not useful
canoniseParser :: Parser CanonData
canoniseParser = do
    try (reserved "Can:")
    x <- choice [ manualVoicesParser ]
    y <- canonTypeParser
    return ((x,y)) -- still not ready to use. canonParser does not hold it yet
---------------------------------------------------------------
-- Voices

-- Fold parsers are not working properly. There seems to be aproblem with all this returns that avoid the voicesParser to fail

manualVoicesParser :: Parser VoicesData
manualVoicesParser = do
    x <- ratioParser  <|> ratioFoldParser <|> return [1.0]
    y <- transpParser  <|> transpFoldParser <|> (return $ defaultTransp x)
    return (propTranspCycle x y)

propTranspCycle:: [Prop] ->  [Transp] -> [(Prop, Transp)]
propTranspCycle prop transp
    | (length prop) == (length transp) = zip prop transp
    | (length prop) > (length transp) = 
        let cycledTransp = take (length prop) $ cycle transp
            zipped = zip prop cycledTransp
        in zipped
    | (length prop) < (length transp) = 
        let cycledProp = take (length transp) $ cycle prop
            zipped = zip cycledProp transp
        in zipped

ratioParser:: Parser [Prop] -- prop of proportion needs to change to ratio
ratioParser = do
    try $ reserved "ra:"
    x <- (sepBy double colon) <|> (return [1.0])
    return (x)

transpParser:: Parser [Transp]
transpParser = do
    try $ reserved "tr:"
    y <- (sepBy double (char '|'))
    return (y)

defaultTransp:: [Double] -> [Double]
defaultTransp x = replicate (length x) 0.0

voicesParser :: String -> Either ParseError VoicesData    
voicesParser x = parse manualVoicesParser "" x

ratioFoldParser:: Parser [Prop] -- prop of proportion needs to change to ratio
ratioFoldParser = do
    oneOf "{"
    optional whiteSpace
    try $ reserved "repeat"
    x <- integer
    y <- double
    try $ colon
    z <- double
    try $ reserved "pattern"
    oneOf "}"
    return (algoTransps (fromIntegral x :: Int) y z)

transpFoldParser:: Parser [Transp]
transpFoldParser = do
    oneOf "{"
    optional whiteSpace
    try $ reserved "repeat"
    x <- integer
    y <- double
    try $ reserved "|"
    z <- double
    try $ reserved "pattern"
    oneOf "}"
    return (algoTransps (fromIntegral x :: Int) y z)

rFuncParser :: String -> Either ParseError [Prop]    
rFuncParser x = parse ratioFoldParser "" x

algoTransps:: Int -> Double -> Double -> [Double]
algoTransps x y z = 
    let interval = z - y
        interList = replicate x interval
        folded = scanl (+) y interList
    in folded

-- a syntax to overwrite individual voices is needed!!!
--------------------------------------------------------------------------------
-- converge or diverge:

convergeParser:: Parser CanonType
convergeParser = do
    try (reserved "cp:")
    cp <- choice [cpIntParser, cpStringParser]
    return (Convergence cp)

cpIntParser:: Parser CP
cpIntParser = do
    cp <- integer
    return (CP (fromIntegral cp:: Int))

cpStringParser:: Parser CP
cpStringParser = do
    choice[
        try $ reserved "last" >> return (CPString "last"),
        try $ reserved "lastx" >> return (CPString "lastx"),
        try $ reserved "palindrome" >> return (CPString "palindrome")
        ]


divergeParser:: Parser CanonType
divergeParser = do
    try (reserved "div")
    percPerTempo <- (sepBy integer (char '%')) -- how can I assign an automatic distribution of percs
    return (Divergence percPerTempo)

canonTypeParser:: Parser CanonType
canonTypeParser = do
    canonType <- choice [ convergeParser, divergeParser]
    return canonType

    -- to test the parser use this function --
pruebaCanonType :: String -> Either ParseError CanonType
pruebaCanonType x = parse canonTypeParser "" x


---------------------------------------------------------------------
    --------- Stream Parser -----------
streamParser :: Parser Streams
streamParser = do
    timbre <- choice [
        try $ sampleParser, 
        try $ waveParser,
        try $ dirtParser
        ] <|> (return (Waveshape ["sin"]))
--    params <- paramsParser <|> (return [("amp", [0.5])]) 
    pattern <- choice [ 
        try $ reserved "iso" >> return ("iso"),
        try $ reserved "isoGrid" >> return ("isoGrid"),
        try $ reserved "eu" >> return ("eu"),
        try $ reserved "rand" >> return ("rand"),
        try $ reserved "rev" >> return ("rev"),
        try $ reserved "revGrid" >> return ("revGrid"),
        try $ reserved "mirr" >> return ("mirr"),
        try $ reserved "pyr" >> return ("pyr")
        ] <|> return ("iso")
    pitchRate <- pitchParser <|> rateParser <|> return (pitchOrRate timbre)
    amp <- ampParser <|> return [0.5]
    return (dirtsynthOrSample timbre pattern pitchRate amp)
 
dirtsynthOrSample:: Timbre -> StreamPattern -> [Double] -> [Amp] -> Streams
dirtsynthOrSample (Waveshape w) x y z = (Synth (Waveshape w) x y z)
dirtsynthOrSample (Samples w) x y z = (Sample (Samples w) x y z)   
dirtsynthOrSample (Dirties w) x y z = (Dirt   (Dirties w) x y z)   


pitchOrRate:: Timbre -> [Double]
pitchOrRate (Waveshape _) = [60.0]
pitchOrRate (Samples _) = [1.0]
pitchOrRate (Dirties _) = [1.0]

        -- test parser
pruebaStream :: String -> Either ParseError Streams
pruebaStream x = parse streamParser "" x

------------ dirtParser--------------
dirtParser:: Parser Timbre
dirtParser = do
    try (reserved "dirts:")
    x <- (sepBy dirtSampleParser comma)
    return (Dirties x)

dirtSampleParser:: Parser String
dirtSampleParser = do
    names <- stringLiteral 
    return (names)

listOfnamesIndexed:: InstName -> [Index] -> [(InstName, Index)]
listOfnamesIndexed ins ixs = 
    let insts = replicate (length ixs) ins
        zipped = zip insts ixs
    in zipped

----------------waveParser -------------------------
waveParser :: Parser Timbre
waveParser = do
    try (reserved "synths:")
    names <- many (choice [ -- works in the Y axis of the canon, each voice recieves on of this instruments
        try $ reserved "sin" >> return ("sin"),
        try $ reserved "saw" >> return ("saw"),
        try $ reserved "sqr" >> return ("sqr"),
        try $ reserved "tri" >> return ("tri"),
        try $ reserved "wave1" >> return ("wave1"),
        try $ reserved "wave2" >> return ("wave2"),
        try $ reserved "wave3" >> return ("wave3")
          ])
    return (Waveshape names)
   
------------------------sampleParser-------------------------------------------
sampleParser :: Parser Timbre
sampleParser = do
    try (reserved "samples:")
    names <- many (choice [
        try $ reserved "bd" >> return ("bd"),
        try $ reserved "clp" >> return ("clp"),
        try $ reserved "oh" >> return ("oh"),
        try $ reserved "oh2" >> return ("oh2"),
        try $ reserved "vibraslap" >> return ("vibraslap"),
        try $ reserved "maracas" >> return ("maracas"),
        try $ reserved "cabasa" >> return ("cabasa"),
        try $ reserved "whistle" >> return ("whistle"),
        try $ reserved "whistle2" >> return ("whistle2"),
        try $ reserved "lowbongo" >> return ("lowbongo"),
        try $ reserved "shortguiro" >> return ("shortguiro"),
        try $ reserved "hibongo" >> return ("hibongo"),
        try $ reserved "csh" >> return ("csh"),
        try $ reserved "sd" >> return ("sd"),
        try $ reserved "hiconga" >> return ("hiconga"),
        try $ reserved "longguiro" >> return ("longguiro"),
        try $ reserved "clh" >> return ("clh"),
        try $ reserved "clh2" >> return ("clh2"),
        try $ reserved "tambourine" >> return ("tambourine"),
        try $ reserved "tambourine2" >> return ("tambourine2")
        ])
    return (Samples names)
   
-------------------pitchParser------------------------------

pitchParser:: Parser Pitches   -- turn values into DB!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -- how to parse -3.0 and -10.0... etc
pitchParser = do
    try $ reserved "pitch:" 
    vals <- many double <|> return [60.0]
    return (vals) 

pruebaPitch :: String -> Either ParseError Pitches
pruebaPitch x = parse pitchParser "" x

rateParser:: Parser Rates   -- turn values into DB!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -- how to parse -3.0 and -10.0... etc
rateParser = do
    try $ reserved "rate:" 
    vals <- many double <|> return [1.0]
    return (vals) 

pruebaRate :: String -> Either ParseError Rates
pruebaRate x = parse pitchParser "" x

------------------------ampParser------------------------------------------------

ampParser:: Parser Amps   -- turn values into DB!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -- how to parse -3.0 and -10.0... etc
ampParser = do
    try (reserved "amp:")
    vals <- many double 
    return (vals) 

pruebaAmp :: String -> Either ParseError Amps
pruebaAmp x = parse ampParser "" x

------------------------------------------------ this below might have to go
paramParser :: Parser Param
paramParser = do
    paramName <- choice [
        try $ reserved "amp:" >> (return "amp"),
        try $ reserved "bla:" >> (return "bla")
        ]
    paramVal  <- many float
    return (paramName, paramVal)

paramsParser :: Parser [Param]
paramsParser = do
    try $ reserved "args"
    params <- paramParser `sepBy` comma
    return params


------------------------------------------
-- transform integers parentesis and negatives into floats
double :: Parser Double
double = do
    x <- choice [
        try $ parens double,
        symbol "-" >> double >>= return . (* (-1)),
        try float,
        try $ fromIntegral <$> integer
        ]
    return x

doublePrueba :: String -> Either ParseError Double
doublePrueba x = parse double "" x



----------------------------------------------------------------------
----------------------------------------------------------------------




theLanguageDef :: P.GenLanguageDef [Char] () Identity
theLanguageDef = P.LanguageDef {
  P.commentStart = "/*",
  P.commentEnd = "*/",
  P.commentLine = "//",
  P.nestedComments = False,
  P.identStart = letter <|> char '_',
  P.identLetter = letter <|> char '_',
  P.opStart = oneOf "",
  P.opLetter = oneOf "",
  P.reservedNames = [ "Temp:","Transp:","db:","iso","isoGrid","eu" ], -- this are words that cannot be variable names
  P.reservedOpNames = [":e:",":r:",":r:",":p:"],
  P.caseSensitive = True
  }
  
myTokenParser :: P.GenTokenParser [Char] () Identity
myTokenParser = P.makeTokenParser theLanguageDef

identifier = P.identifier myTokenParser
reserved = P.reserved myTokenParser
operator = P.operator myTokenParser
reservedOp = P.reservedOp myTokenParser
charLiteral = P.charLiteral myTokenParser
stringLiteral = P.stringLiteral myTokenParser
natural = P.natural myTokenParser
integer = P.integer myTokenParser
float = P.float myTokenParser
naturalOrFloat = P.naturalOrFloat myTokenParser
decimal = P.decimal myTokenParser
hexadecimal = P.hexadecimal myTokenParser
octal = P.octal myTokenParser
symbol = P.symbol myTokenParser
lexeme = P.lexeme myTokenParser
whiteSpace = P.whiteSpace myTokenParser
parens = P.parens myTokenParser
braces = P.braces myTokenParser
angles = P.angles myTokenParser
brackets = P.brackets myTokenParser
squares = P.squares myTokenParser
semi = P.semi myTokenParser
comma = P.comma myTokenParser
colon = P.colon myTokenParser
dot = P.dot myTokenParser
semiSep = P.semiSep myTokenParser
semiSep1 = P.semiSep1 myTokenParser
commaSep = P.commaSep myTokenParser
commaSep1 = P.commaSep1 myTokenParser