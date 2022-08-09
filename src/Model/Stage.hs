-- |
-- Module      : Model.Stage
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- A stage of the CLI game.

{-# LANGUAGE OverloadedStrings #-}

module Model.Stage
  ( SectionId,
    StageId,
    Section,
    Stage,
    StageStatus (..),
    playSection,
    stageStatus,
  )
where

import qualified Data.Map.Strict as M
import           Data.Yaml.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import           Model.Progress  (Progress, SectionId, StageId, initialProgress,
                                  reached, stageSection)
import           Model.Step      (Step, playStep)
import           Model.Warn      (Warn (..))
import           UI.UI           (UI)

-- | A stage of a game which the player can visit.
data Stage =
  Stage
    { name     :: String,
      sections :: M.Map SectionId Section
    }

-- | A section of a stage of a game, consisting of a sequence of steps
-- and some requirements.
data Section = Section
  { requirements :: Progress,
    steps        :: [Step]
  }
  deriving (Show)

-- | The status in which a stage can be at some point.
data StageStatus
  = NextSection (SectionId, Section) -- ^ The next section that should be played in the stage
  | Blocked                          -- ^ The stage is blocked
  | Finished                         -- ^ The stage has been finished

instance Show Stage where
  show = name

instance Warn Stage where
  warnings stage
    | M.null $ sections stage = ["Stage with no sections"]
    | otherwise = concatMap warnings $ sections stage

instance Warn Section where
  warnings section
    | null $ steps section = ["Section with no steps"]
    | otherwise = concatMap warnings $ steps section

instance FromJSON Stage where
  parseJSON = withObject "stage" $ \s ->
    Stage
      <$> s .: "name"
      <*> s .: "sections"

instance FromJSON Section where
  parseJSON = withObject "section" $ \s ->
    Section
      <$> (s .:? "requires" .!= initialProgress)
      <*> s .: "steps"

-- | Play a section in the UI monad.
playSection :: UI m => Section -> m ()
playSection = mapM_ playStep . steps

-- | Compute the status of a stage given a progress state.
stageStatus :: Progress -> (StageId, Stage) -> StageStatus
stageStatus current stage =
  case nextSection current stage of
    Nothing -> Finished
    Just sectionPair@(_, section) ->
      if reached current (requirements section)
        then NextSection sectionPair
        else Blocked

nextSection :: Progress -> (StageId, Stage) -> Maybe (SectionId, Section)
nextSection progress (stageId, stage) =
  case stageSection stageId progress of
    Just sectionId -> M.lookupGT sectionId $ sections stage
    Nothing        -> M.lookupMin $ sections stage
