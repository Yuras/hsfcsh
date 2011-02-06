module Main
(
main
)
where

import Prelude hiding (catch)
import Network
import System.IO
import System.Process
import System.Environment
import System.Console.GetOpt
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.Maybe
import Data.Char

data Options = Options {
  optHsfcsh :: String,
  optPort :: PortNumber
} deriving Show

defaultOptions = Options {
  optHsfcsh = "hsfcsh",
  optPort = 1234
}

options :: [OptDescr (Options -> Options)]
options = [
  Option ['c'] ["with-hsfcsh"] (ReqArg (\o opts -> opts {optHsfcsh = o}) "CMD") "hsfcsh command",
  Option ['p'] ["port"] (ReqArg (\o opts -> opts {optPort = fromIntegral $ read o}) "PORT") "port to cennect to"
 ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo header options)

header :: String
header = "Usage: hsfcsh_do [OPTION...] (spawn | exit | compile \"ARGS\")"

main :: IO ()
main = withSocketsDo main'

main' = do
  args <- getArgs
  (opts, args) <- parseOpts args
  case length args of
    1 -> case head args of
           "spawn" -> spawn (optHsfcsh opts)
           "exit" -> do
                       h <- connect (optPort opts)
                       let handle = fromJust h
                       when (isJust h) $ hPutStrLn handle "q" >> hFlush handle >> hClose handle
           _ -> putStrLn $ usageInfo header options
    2 -> case head args of
           "compile" -> do
                       handle <- connectOrSpawn (optHsfcsh opts) (optPort opts)
                       let tgt = args!!1
                       id <- findId handle tgt
                       let id' = fromJust id
                       if isNothing id
                         then do
                                putStrLn "id not found, will assing"
                                hPutStr handle $ "t\nmxmlc " ++ tgt ++ "\n"
                                hFlush handle
                          else do
                                putStrLn $ "found id = " ++ show id'
                                hPutStr handle $ "t\n" ++ "compile " ++ show id' ++ "\n"
                                hFlush handle
                       readRes handle
                       hPutStrLn handle "n"
                       hFlush handle
                       hClose handle
           _ -> putStrLn $ usageInfo header options
    _ -> putStrLn $ usageInfo header options

findId :: Handle -> String -> IO (Maybe Int)
findId handle tgt = do
  hPutStr handle "t\ninfo\n"
  hFlush handle
  res <- readRes' handle
  print res
  return $ fnd res
  where
  fnd :: [String] -> Maybe Int
  fnd [] = Nothing
  fnd [_] = Nothing
  fnd (x1:x2:xs) =
    if trim tgt' == trim tgt
      then Just id
      else fnd xs
    where
    id :: Int
    id = read $ drop 4 x1
    tgt' = drop 7 x2

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

readRes :: Handle -> IO ()
readRes handle = lp
  where
  lp = do
    l <- hGetLine handle
    if l == "<FCSH END>"
      then return ()
      else putStrLn l >> hFlush stdout >> lp

readRes' :: Handle -> IO [String]
readRes' handle = lp []
  where
  lp :: [String] -> IO [String]
  lp ls = do
    l <- hGetLine handle
    if l == "<FCSH END>"
      then return (reverse ls)
      else lp (l:ls)

connectOrSpawn :: String -> PortNumber -> IO Handle
connectOrSpawn cmd port = do
  h <- connect port
  if isNothing h
    then spawn cmd >> connect port >>= return . fromJust
    else return $ fromJust h

connect :: PortNumber -> IO (Maybe Handle)
connect port = catch (fmap Just (connectTo "localhost" (PortNumber port))) (\e -> print (e :: SomeException) >> return Nothing)

spawn :: String -> IO ()
spawn cmd = runCommand cmd >> yield >> threadDelay 2000000

