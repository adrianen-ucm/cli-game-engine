-- |
-- Module      : Model.Game
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- API of the model of a CLI game.

{-# LANGUAGE OverloadedStrings #-}

module Model.Game
  ( Game,
    StageId,
    Progress,
    StageStatus (..),
    playSection,
    welcome,
    warnings,
    goodbye,
    initialProgress,
    updateProgress,
    pickStage,
    nextSection,
    isGameFinished,
    finishedStageAlert,
    blockedStageAlert,
    fromYamlFileThrow
  )
where

import           Data.Bifunctor   (second)
import           Data.Foldable    (forM_)
import qualified Data.Map.Strict  as M
import           Data.Maybe       (isNothing, mapMaybe)
import           Data.Yaml.Aeson  (FromJSON (..), decodeFileThrow, withObject,
                                   (.:), (.:?))
import           I18n.Applicative (ApplicativeI18n (..))
import           Model.Progress   (Progress, initialProgress, stageSection,
                                   updateProgress)
import           Model.Stage      (Section, Stage, StageId, StageStatus (..),
                                   playSection, stageStatus)
import           Model.Warn       (Warn (..))
import           UI.UI            (UI, option, tell)

-- | Game ROM data.
data Game =
  Game
    { name   :: String,
      intro  :: Maybe Section,
      stages :: M.Map StageId Stage
    }
  deriving (Show)

instance Warn Game where
  warnings game
    | M.null (stages game) && isNothing (intro game) = ["Game with no stages"]
    | otherwise = concat
      [ maybe [] warnings (intro game)
      , concatMap warnings (stages game)
      , simulationWarnings game
      ]

instance FromJSON Game where
  parseJSON = withObject "game" $ \g ->
    Game
      <$> (g .:  "name")
      <*> (g .:? "intro")
      <*> (g .:  "stages")

-- | Load the game from a YAML file, throwing an
-- exception if its data does not correspond to a valid
-- game.
fromYamlFileThrow :: FilePath -> IO Game
fromYamlFileThrow = decodeFileThrow

-- | Check if a game is finished, given a player
-- progress.
isGameFinished :: Game -> Progress -> Bool
isGameFinished game progress =
  all (isStatusFinished . checkStatus) gameStages
  where
    gameStages = M.toList $ stages game
    checkStatus = stageStatus progress
    isStatusFinished Finished = True
    isStatusFinished _        = False

-- | Next section that should be played in a stage, given a
-- player progress.
nextSection :: StageId -> Game -> Progress -> StageStatus
nextSection stageId game progress =
  case M.lookup stageId $ stages game of
    Nothing    -> Finished
    Just stage -> stageStatus progress (stageId, stage)

-- | Show an introductory message and play
-- the game intro section.
welcome :: UI m => Game -> m ()
welcome game = do
  gameLoaded (name game) >>= tell
  forM_ (intro game) playSection

-- | Show an ending message in the UI monad.
goodbye :: UI m => Game -> m ()
goodbye _ =
  gameOver >>= tell

-- | Show a finished stage message in the UI monad.
finishedStageAlert :: UI m => m ()
finishedStageAlert =
  nothingToDo >>= tell

-- | Show a blocked stage message in the UI monad.
blockedStageAlert :: UI m => m ()
blockedStageAlert =
  blockedStage >>= tell

-- | Ask the user to select a stage in the UI monad.
pickStage :: UI m => Game -> Progress -> m StageId
pickStage game progress =
  selectStage >>= option describedStages
  where
    isAvailable = availableAtMenu progress
    availableStages = filter isAvailable $ M.toList $ stages game
    describedStages = map (second show) availableStages

availableAtMenu :: Progress -> (StageId, Stage) -> Bool
availableAtMenu progress stagePair@(stageId, _) =
  case (stageSection stageId progress, stageStatus progress stagePair) of
    (Just _, Blocked)  -> True
    (_, NextSection _) -> True
    _                  -> False

simulationWarnings :: Game -> [String]
simulationWarnings game =
  if M.null unfinished
    then []
    else
      [ "With progress " <> show progress <> " the following stages are blocked:",
        '\t' : show (M.keys unfinished)
      ]
  where
    (unfinished, progress) = simulateGameLoop (stages game, initialProgress)

simulateGameLoop :: (M.Map StageId Stage, Progress) -> (M.Map StageId Stage, Progress)
simulateGameLoop initial@(stgs, progress) =
  if null nextStages
    then initial
    else simulateGameLoop (M.filterWithKey filterNoFinished stgs, updatedProgress)
  where
    withStatus s@(stgId, _) = (stgId, stageStatus progress s)
    withNext (stgId, NextSection section) = Just (stgId, section)
    withNext (_, _)                       = Nothing
    nextStages = mapMaybe (withNext . withStatus) $ M.toList stgs
    updatedProgress = updateProgress (stageId, sectionId) progress
    (stageId, (sectionId, _)) = head nextStages
    isNotFinished Finished = False
    isNotFinished _        = True
    filterNoFinished stgId stage = isNotFinished $ stageStatus updatedProgress (stgId, stage)
