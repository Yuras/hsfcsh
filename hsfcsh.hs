#!/usr/bin/runhaskell
module Main
(
main
)
where

import Prelude hiding (catch)
import Network
import System.Exit
import System.IO
import System.Process
import System.Posix.Daemonize
import System.Log.Logger
import System.Log.Handler.Syslog
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe

log_ :: String
log_ = "hsfcsh"

main :: IO ()
main = do
  daemonize $ do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  s <- openlog "hsfcsh" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [s])
  infoM log_ "started"
  fcsh <- runInteractiveCommand "fcsh"

  threadId <- myThreadId
  chan <- newChan
  forkIO $ reader (getOut fcsh) chan threadId
  forkIO $ reader (getErr fcsh) chan threadId

  waitReady chan Nothing

  withSocketsDo $ main' fcsh chan
  terminateProcess $ getPH fcsh
  infoM log_ "shutdown"

mPutStrLn :: Maybe Handle -> String -> IO ()
mPutStrLn Nothing _ = return ()
mPutStrLn (Just handle) str = hPutStrLn handle str >> hFlush handle

waitReady :: Chan Char -> Maybe Handle -> IO ()
waitReady chan handle = waitReady' []
  where
  waitReady' cs = do
    c <- readChan chan
    if c == '\n'
      then mPutStrLn handle (reverse cs) >> waitReady' []
      else if (c:cs) == " )hscf("
             then mPutStrLn handle "<FCSH END>"
             else waitReady' (c:cs)

main' :: FCSH -> Chan Char -> IO ()
main' fcsh chan = do
  sock <- listenOn (PortNumber 1234)
  acceptLoop fcsh sock chan
  sClose sock

type FCSH = (Handle, Handle, Handle, ProcessHandle)

getIn :: FCSH -> Handle
getIn (i, _, _, _) = i

getOut :: FCSH -> Handle
getOut (_, o, _, _) = o

getErr :: FCSH -> Handle
getErr (_, _, e, _) = e

getPH :: FCSH -> ProcessHandle
getPH (_, _, _, p) = p

acceptLoop :: FCSH -> Socket -> Chan Char -> IO ()
acceptLoop fcsh sock chan = do
  (handle, host, _) <- accept sock
  infoM log_ $ "accept from " ++ host
  hSetBinaryMode handle False
  hSetNewlineMode handle universalNewlineMode
  cont <- catch (serve fcsh handle chan) (\e -> warningM log_ (show (e :: SomeException)) >> return True)
  hClose handle
  when cont $ acceptLoop fcsh sock chan

serve :: FCSH -> Handle -> Chan Char -> IO Bool
serve fcsh handle chan = do
  cmd <- hGetLine handle
  case cmd of
    "q" -> return False
    "t" -> do
             tgt <- hGetLine handle
             debugM log_ $ "command: " ++ tgt
             hPutStrLn (getIn fcsh) $ tgt
             hFlush (getIn fcsh)
             waitReady chan (Just handle)
             serve fcsh handle chan
    "n" -> debugM log_ "NOP" >> return True
    _ -> warningM log_ ("unknown command :" ++ cmd) >> return True

reader :: Handle -> Chan Char -> ThreadId -> IO ()
reader handle chan mainThread = do
  hSetBinaryMode handle False
  let lp h = catch (hGetChar h) (\e -> errorM log_ (show (e :: SomeException)) >> killThread mainThread >> undefined) >>= writeChan chan >> lp h
  lp handle

