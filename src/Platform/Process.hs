-- |
-- Module      : Platform.GameProcess
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- Computations in the GameStation monad for playing a CLI game.

{-# LANGUAGE Rank2Types #-}

module Platform.Process (playGame) where

import           Control.Monad              (unless, when)
import           Control.Monad.Reader       (MonadReader (ask))
import           Control.Monad.State.Strict (get, modify)
import           Model.Game                 (StageId, StageStatus (..),
                                             blockedStageAlert,
                                             finishedStageAlert, goodbye,
                                             isGameFinished, nextSection,
                                             pickStage, playSection,
                                             updateProgress, welcome)
import           Platform.GameStation       (GameStation)

-- | Shows the game intro, plays the game
-- and finally shows a goodbye message.
playGame :: GameStation ()
playGame = do
  ask >>= welcome
  playGameLoop
  ask >>= goodbye

-- | Shows the stage selection menu and plays the
-- selected stage until the game is finished.
playGameLoop :: GameStation ()
playGameLoop = do
  rom <- ask
  ram <- get
  unless (isGameFinished rom ram) $ do
    stageId <- pickStage rom ram
    playStageLoop True stageId
    playGameLoop

-- | Plays all the remaining stage sections allowed
-- by the current game progress.
playStageLoop :: Bool -> StageId -> GameStation ()
playStageLoop isFirst stageId = do
  rom <- ask
  ram <- get
  case nextSection stageId rom ram of
    Finished -> when isFirst finishedStageAlert
    Blocked -> when isFirst blockedStageAlert
    NextSection (sectionId, section) -> do
      playSection section
      modify $ updateProgress (stageId, sectionId)
      playStageLoop False stageId
