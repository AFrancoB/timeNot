module Sound.TimeNot.REPL where

import  qualified Sound.OSC as SOSC
import Sound.OSC.Transport.FD.UDP
import Sound.OSC.Transport.FD as FD
import Sound.OSC.Transport.FD as OSC
import Control.Monad
import Control.Concurrent
import Data.List
import Network.Socket (SockAddr(..),tupleToHostAddress)
import Data.Bifunctor
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Time as T
import Data.Either

import Sound.TimeNot.AST
import Sound.TimeNot.Parsers
import Sound.TimeNot.ToEvents


scheduler :: UDP -> MVar [Event] -> IO ()
scheduler udp elMVar = do
  now <- T.getCurrentTime
  let f e = T.diffUTCTime (time e) now < 0.1 -- gets the time of the events in UTC
  el <- takeMVar elMVar    -- put out the elements of the mvar to analyse them in next line
  let (nearEvents,distantEvents) = partition f el  -- divides events in near and distant
  putMVar elMVar distantEvents -- adds distant events to the existing ones
  eventsToSuperCollider udp nearEvents -- ¿? ¿? ¿?
  --putStrLn $ show (length nearEvents) ++ " events out"
  threadDelay 100000 -- do every 0.1 secs
  scheduler udp elMVar --recursion

eventsToSuperCollider :: UDP -> [Event] -> IO () -- this event already ahs a UTC time
eventsToSuperCollider udp events = do
  let readyToSend = map (eventTime) events
  sent <- sendEvents udp readyToSend
  return (sent)
  

eventTime:: Event -> (Int, Int, Event)
eventTime e = (i1,i2,e)
  where (i1,i2) = utcTimeToSplitPosixTime (time e)
      
sendEvent:: UDP -> (Int,Int,Event) ->  IO()
sendEvent udp (secs,mSecs,(Event v w x y z n p)) = sendTo udp p a
  where 
    p = SOSC.p_message "/canon" [SOSC.int32 secs, SOSC.int32 mSecs, SOSC.double w, SOSC.double x, SOSC.string y, SOSC.double z]
    a = SockAddrInet 57120 $ tupleToHostAddress (127,0,0,1)

sendEvents :: UDP -> [(Int, Int, Event)] -> IO ()
sendEvents udp events = do
  sequence $ fmap (sendEvent udp) events
  return ()

sendError:: UDP -> String ->  IO()
sendError udp string = sendTo udp er a
  where 
    er = SOSC.p_message "/printError" [SOSC.string string]
    a = SockAddrInet 57120 $ tupleToHostAddress (127,0,0,1)


utcTimeToSplitPosixTime :: T.UTCTime -> (Int,Int)
utcTimeToSplitPosixTime t = (seconds,microseconds)
  where
    posixTime = utcTimeToPOSIXSeconds t
    seconds = floor posixTime 
    microseconds = round ((posixTime - (realToFrac seconds :: T.NominalDiffTime )) * 1000000)

repl :: UDP -> MVar [Event] -> IO ()
repl udp elMVar = do
  m <- FD.recvMessage udp -- recieves message
  let m' = messageToProgram m -- transforms message to program
  now <- T.getCurrentTime  -- get current time
  eitherErrorOrProgram udp elMVar now m'
  repl udp elMVar

eitherErrorOrProgram :: UDP -> MVar [Event] -> T.UTCTime -> Either String Program -> IO ()
eitherErrorOrProgram udp elMVar now (Left m) = {- putStrLn m -}do
  putStrLn m
  sendError udp m
  return ()
eitherErrorOrProgram udp elMVar now (Right p) = do
  let es = render now p
  putStrLn $ show (length $ es now now) 
  existing <- takeMVar elMVar
  putMVar elMVar $ existing ++ es now now -- these two nows need to change!!!!


messageToProgram :: Maybe SOSC.Message -> Either String Program
messageToProgram (Just (SOSC.Message "/evaluate" [])) = Left "error, empty message ¿ ?"
messageToProgram (Just (SOSC.Message "/evaluate" (d:[]))) | SOSC.datum_tag d == 's' = evaluate (SOSC.ascii_to_string $ SOSC.d_ascii_string d)

evaluate:: String -> Either String Program 
evaluate x = first (show) (runCanonParser x)

