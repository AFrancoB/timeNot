{-# LANGUAGE OverloadedStrings #-}
module Sound.TimeNot.MapEstuary where 

import Sound.OSC
import Data.Map.Strict
import qualified Data.Text as Tx
import Data.Text (Text)
import Data.Time

import Sound.TimeNot.AST
import Sound.TimeNot.Render
import Sound.TimeNot.REPL



mapForEstuary:: Event -> (UTCTime, Map Text Datum)
mapForEstuary (Event ti sus pi inst amp n pan a b c d) =
    let tiempo = ti
        mapa = fromList [("cut", double sus), ("speed", double pi), ("sample_name", string inst), ("sample_n", string "0"), ("gain", double amp)]
    in (tiempo, mapa)