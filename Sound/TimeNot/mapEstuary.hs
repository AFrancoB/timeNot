{-# LANGUAGE OverloadedStrings #-}
module Sound.TimeNot.Mapa where 

import Sound.OSC
import Data.Map.Strict
import qualified Data.Text as Tx
import Data.Text (Text)
import Data.Time

import Sound.TimeNot.AST
import Sound.TimeNot.ToEvents
import Sound.TimeNot.REPL

timeNot:: UTCTime  ->  Text -> Either Text [(UTCTime, Map Text Datum)]
timeNot now toEval = 
    let eval = evaluate $ Tx.unpack toEval
        program = errorOrEventsEstuary now eval
    in program

errorOrEventsEstuary:: UTCTime -> Either String Program -> Either Text [(UTCTime, Map Text Datum)]
errorOrEventsEstuary now (Left m) = Left $ "Error at evaluation"
errorOrEventsEstuary now (Right p) = 
    let e = progToEvents now p -- :: [Event]
        mapping = Prelude.map (mapEvent) e
    in Right mapping


mapEvent:: Event -> (UTCTime, Map Text Datum)
mapEvent (Event ti sus pi inst amp) =
    let tiempo = ti
        mapa = fromList [("cut", double sus), ("speed", double pi), ("sample_name", string inst), ("sample_n", string "0"), ("gain", double amp)]
    in (tiempo, mapa)