#!/usr/bin/runhaskell
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
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.Maybe
import Data.Char

main :: IO ()
main = withSocketsDo main'

main' = do
  args <- getArgs
  print args
  case length args of
    1 -> case head args of
           "spawn" -> spawn
           "exit" -> do
                       h <- connect
                       let handle = fromJust h
                       when (isJust h) $ hPutStrLn handle "q" >> hClose handle
           _ -> return ()
    2 -> case head args of
           "compile" -> do
                       handle <- connectOrSpawn
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
           _ -> return ()
    _ -> return ()

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

connectOrSpawn :: IO Handle
connectOrSpawn = do
  h <- connect
  if isNothing h
    then spawn >> connect >>= return . fromJust
    else return $ fromJust h

connect :: IO (Maybe Handle)
connect = catch (fmap Just (connectTo "localhost" (PortNumber 1234))) (\e -> print (e :: SomeException) >> return Nothing)

spawn :: IO ()
spawn = runCommand "hsfcsh" >> yield >> threadDelay 2000000

