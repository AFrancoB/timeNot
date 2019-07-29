module Main where

import qualified Sound.TimeNot.REPL as REPL

import Sound.OSC.Transport.FD.UDP
import Control.Concurrent

main :: IO ()
main = do
  udp <- udpServer "127.0.0.1" 57300
  elMVar <- newMVar []
  forkIO $ REPL.scheduler udp elMVar
  forkIO $ REPL.repl udp elMVar
  return ()


-- main:: IO()
-- main = loop
--  where
--   loop = do
--     putStrLn "listen - to start the program. exit - to exit the program"
--     s <- getLine
--     if s == "listen"
--         then 
--             if s == "exit"
--                 then return ()
--                 else do forkIO $ REPL.main
--                         loop
--         else return ()