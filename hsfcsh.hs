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
import System.Environment
import System.Posix.Daemonize
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Console.GetOpt
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe

data Options = Options {
  optFcshCmd :: String,
  optPort :: PortNumber,
  optDaemonize :: Bool,
  optSyslog :: Bool
} deriving Show

defaultOptions = Options {
  optFcshCmd = "fcsh",
  optPort = 1234,
  optDaemonize = True,
  optSyslog = True
}

options :: [OptDescr (Options -> Options)]
options = [
  Option ['c'] ["with-fcsh"] (ReqArg (\o opts -> opts {optFcshCmd = o}) "CMD") "fcsh command",
  Option ['p'] ["port"] (ReqArg (\o opts -> opts {optPort = fromIntegral $ read o}) "PORT") "port to listen on",
  Option ['f'] ["foreground"] (NoArg (\opts -> opts {optDaemonize = False})) "don't daemonize",
  Option ['d'] ["daemonize"] (NoArg (\opts -> opts {optDaemonize = True})) "daemonize",
  Option ['s'] ["silent"] (NoArg (\opts -> opts {optSyslog = False})) "don't log to the syslog",
  Option ['v'] ["verbose"] (NoArg (\opts -> opts {optSyslog = True})) "log to the syslog"
 ]

log_ :: String
log_ = "hsfcsh"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo header options)
  where
  header = "Usage: hsfcsh [OPTION...]"

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  (if optDaemonize opts then daemonize else id) $ do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  s <- openlog "hsfcsh" [PID] USER DEBUG
  when (optSyslog opts) $ updateGlobalLogger rootLoggerName (setHandlers [s])
  infoM log_ "started"
  fcsh <- runInteractiveCommand $ optFcshCmd opts

  threadId <- myThreadId
  chan <- newChan
  forkIO $ reader (getOut fcsh) chan threadId
  forkIO $ reader (getErr fcsh) chan threadId

  waitReady chan Nothing

  withSocketsDo $ main' opts fcsh chan
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

main' :: Options -> FCSH -> Chan Char -> IO ()
main' opts fcsh chan = do
  sock <- listenOn (PortNumber $ optPort opts)
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

