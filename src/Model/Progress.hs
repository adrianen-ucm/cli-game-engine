-- |
-- Module      : Model.Progress
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- Player progress in a CLI game.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Progress
  ( StageId,
    SectionId,
    Progress,
    initialProgress,
    stageSection,
    updateProgress,
    reached,
  )
where

import           Data.Aeson.Types (FromJSONKey)
import qualified Data.Map.Strict  as M
import           Data.Yaml.Aeson  (FromJSON)

-- | A stage identifier.
newtype StageId = StageId Int
  deriving
    ( Eq,
      Ord,
      Show,
      FromJSON,
      FromJSONKey
    )

-- | A section identifier.
newtype SectionId = SectionId Int
  deriving
    ( Eq,
      Ord,
      Show,
      FromJSON,
      FromJSONKey
    )

-- | Player progress state. It stores the last completed
-- section of each played stage.
newtype Progress =
  Progress
    { progress :: M.Map StageId SectionId
    }
  deriving (FromJSON)

instance Show Progress where
  show =
    show . map pairDescription . M.toList . progress
    where
      pairDescription (stage, section) = show stage <> ": " <> show section

-- | An empty game progress.
initialProgress :: Progress
initialProgress =
  Progress M.empty

-- | Get the last section completed of a stage.
stageSection :: StageId -> Progress -> Maybe SectionId
stageSection stageId =
  M.lookup stageId . progress

-- | Update the last section completed of a stage.
updateProgress :: (StageId, SectionId) -> Progress -> Progress
updateProgress (stageId, sectionId) =
  Progress . M.insert stageId sectionId . progress

-- | Check if a game progress state has been reached, compared
-- to another given progress state.
reached :: Progress -> Progress -> Bool
reached current =
  all checkRequirement . M.toList . progress
  where
    checkRequirement (stageId, sectionId) =
      case M.lookup stageId $ progress current of
        Nothing               -> False
        Just currentSectionId -> currentSectionId >= sectionId
