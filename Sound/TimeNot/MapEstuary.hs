{-# LANGUAGE OverloadedStrings #-}
module Sound.TimeNot.MapEstuary where 

import Sound.OSC
import Data.Map.Strict
import qualified Data.Text as Tx
import Data.Text (Text)
import Data.Time

import Sound.TimeNot.AST
import Sound.TimeNot.ToEvents
import Sound.TimeNot.REPL

--MapEstuary.hs
timeNot:: UTCTime-> UTCTime -> UTCTime -> Text -> Either Text [(UTCTime, Map Text Datum)]
timeNot oTime wStart wEnd text =
    let eval = evaluate $ Tx.unpack text
        program = errorOrEventsEstuary oTime wStart wEnd eval
    in program

errorOrEventsEstuary:: UTCTime -> Either String Program -> UTCTime -> UTCTime -> Either Text [(UTCTime, Map Text Datum)]
errorOrEventsEstuary oTime (Left m) wStart wEnd = Left $ "Error at evaluation"
errorOrEventsEstuary oTime (Right p) wStart wEnd = 
    let e = progToEvents oTime p wSTart wEnd -- :: [Event]
        mapping = Prelude.map (mapEvent) e
    in Right mapping

mapEvent:: Event -> (UTCTime, Map Text Datum)
mapEvent (Event ti sus pi inst amp) =
    let tiempo = ti
        mapa = fromList [("cut", double sus), ("speed", double pi), ("sample_name", string inst), ("sample_n", string "0"), ("gain", double amp)]
    in (tiempo, mapa)