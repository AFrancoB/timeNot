module Sound.TimeNot.AST where

import Data.Time
-- Datatypes

-- For Duration Parser
type CanonDuration = Double
type CanonDurations = [CanonDuration]
type Loop = Bool
-- For Metric Depth Parser
type MetricDepth = Double

type EventF = UTCTime -> UTCTime -> [Event] 

-- semi-lazy Period
 --         Events  originTime   PeriodDuration
type SLP = ([Event], UTCTime, NominalDiffTime)

type SLPPure = ([PureEvent], UTCTime, NominalDiffTime)


-- For Onset Pattern Parser
type OnsetAttack = Bool
type OnsetCP = Bool
type Onset = (OnsetAttack,OnsetCP)
data OnsetPattern = Onsets [Onset] deriving (Show)

-- I have no clue what this is
type VoiceIndex = Int

-- For Canon Type Parser
data CP = CP Int | CPString String deriving (Show)
data CanonType = Convergence CP | Divergence [Integer] deriving (Show)
type CanonData = (VoicesData, CanonType)

-- For Voices Parser
type Prop = Double
type Transp = Double 
type VoiceData = (Prop,Transp) 
type VoicesData = [VoiceData]

-- For Sound Parser
-- strings for the names of the waves and the samples
type Param = (String, [Double])
type Params = [Param]
type Index = Int
type InstName = String
type InstNames = [String]
type DirtInst = (InstName, Index) -- not implemented yet
type StreamPattern = String
data Timbre = Waveshape InstNames | Samples InstNames | Dirties InstNames deriving (Show)
data Streams = 
  Synth Timbre StreamPattern Pitches Amps 
  | Sample Timbre StreamPattern Rates Amps 
  | Dirt Timbre StreamPattern Rates Amps  deriving (Show)
     

-- canonic function Data types
type OnsetDur = Double -- ¿¿¿wtf is this???

type LeEvent = Double -- The duration for each event
type LeEvents = [LeEvent] 
type Time = Double  -- the offset time for each event
type Times = [Time]

type Pitch = Double   -- Pitch, for transposition and for pitch
type Pitches = [Pitch]

type Rate = Double
type Rates = [Rate]

type Amp = Double
type Amps = [Amp]

type Instrument = String  -- wtf is this? why?
type Instruments = [InstNames]   -- crap


-- Event is what is sent as an OSC message to SC
data Event = Event {
  time :: UTCTime,
  lengthEvent :: LeEvent, 
  pitch :: Pitch,
  instrument :: Instrument,
  amp :: Amp
  -- there should be out and pan as well
} deriving (Show)

data PureEvent = PEvent {
  ptime :: Double,
  plengthEvent :: LeEvent, 
  ppitch :: Pitch,
  pinstrument :: Instrument,
  pamp :: Amp
  -- there should be out and pan as well
} deriving (Show)

data Canon = Canon {

  clength :: ([CanonDuration],Loop),

  onsetPattern :: OnsetPattern,

  voices :: VoicesData, -- solo hace voces manualmente, agregar parser algoritmico

  canonType :: CanonType, -- done

  streams :: Streams

  } deriving (Show)

data TiempoChange = BPM Double | CPS Double deriving (Show)

data Expression = RunTempo TiempoChange | RunCanon Canon deriving (Show)
  
type Program = [Expression]