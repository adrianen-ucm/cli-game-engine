-- |
-- Module      : UI.IOConfig
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- Different IO setups for the CLI game UI.

module UI.IOConfig
  ( withInteractionDisabled,
    withInteractionEnabled,
  )
where

import           Control.Exception (bracket)
import           System.IO         (BufferMode (LineBuffering, NoBuffering),
                                    hGetBuffering, hGetEcho, hSetBuffering,
                                    hSetEcho, stdin, stdout)

data IOConfig =
  IOConfig
    { echo   :: Bool,
      bufIn  :: BufferMode,
      bufOut :: BufferMode
    }

-- | Execute an IO computation within a no interactive context
-- and then restore the previous setup.
withInteractionDisabled :: IO a -> IO a
withInteractionDisabled =
  withConfig $ IOConfig False NoBuffering NoBuffering

-- | Execute an IO computation within an interactive context
-- and then restore the previous setup.
withInteractionEnabled :: IO a -> IO a
withInteractionEnabled =
  withConfig $ IOConfig True LineBuffering LineBuffering

withConfig :: IOConfig -> IO a -> IO a
withConfig config action =
  bracket currentIOConfig applyIOConfig configAction
  where
    configAction = const $ applyIOConfig config >> action

currentIOConfig :: IO IOConfig
currentIOConfig =
  IOConfig
    <$> hGetEcho stdin
    <*> hGetBuffering stdin
    <*> hGetBuffering stdout

applyIOConfig :: IOConfig -> IO ()
applyIOConfig config = do
  hSetEcho stdin $ echo config
  hSetBuffering stdin $ bufIn config
  hSetBuffering stdout $ bufOut config
