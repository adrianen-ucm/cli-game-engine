-- |
-- Module      : UI.IO
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- IO utilities for the CLI game UI.

module UI.IO (clearScreen, getLineInt, slowPutStrLn) where

import           Control.Concurrent (MVar, forkIO, killThread, newMVar,
                                     readMVar, swapMVar, threadDelay)

-- | Clear terminal by using ANSI characters.
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Safely read an Int from a line.
getLineInt :: IO (Maybe Int)
getLineInt = do
  line <- getLine
  pure $ case reads line of
    [(n, "")] -> Just n
    _         -> Nothing

-- | Print a string slowly, character by character,
-- with a skip listener (i.e. immediate print).
slowPutStrLn :: String -> IO ()
slowPutStrLn message = do
  skip <- newMVar False
  skipperId <- forkIO $ skipListener skip
  slowPutStrLnLoop skip message
  killThread skipperId

skipListener :: MVar Bool -> IO ()
skipListener skip = do
  _ <- getChar
  _ <- swapMVar skip True
  skipListener skip

slowPutStrLnLoop :: MVar Bool -> String -> IO ()
slowPutStrLnLoop _ [] = putChar '\n'
slowPutStrLnLoop skip ccs@(c : cs) = do
  shouldSkip <- readMVar skip
  if shouldSkip
    then putStrLn ccs
    else do
      putChar c
      threadDelay 40000
      slowPutStrLnLoop skip cs
