-- |
-- Module      : Model.Step
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- A single step of the CLI game.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Model.Step (Step, Choice, playStep) where

import           Control.Monad   (unless)
import           Data.Yaml.Aeson (FromJSON (..), parseMaybe, withObject, (.!=),
                                  (.:), (.:?))
import           Model.Warn      (Warn (..))
import           UI.UI           (UI, optionPanel, tell)

-- | A single step of a game.
data Step
  = Speak String String -- ^ A character telling something
  | StoryTeller String  -- ^ The storyteller telling something
  | Decision [Choice]   -- ^ The player has to decide among some options
  deriving (Show)

data Choice =
  Choice
    { correct  :: Bool,
      text     :: String,
      subSteps :: [Step]
    }

instance Show Choice where
  show = text

instance Warn Step where
  warnings (Decision choices)
    | null choices = ["Decision with no choices"]
    | not $ any correct choices = ["Decision with no correct choice"]
    | otherwise = concatMap warnings choices
  warnings _ = []

instance Warn Choice where
  warnings = concatMap warnings . subSteps

instance FromJSON Step where
  parseJSON = withObject "step" $ \s ->
    case parseMaybe @_ @String (.: "type") s of
      Just "speak"       -> Speak <$> s .: "character" <*> s .: "text"
      Just "storyteller" -> StoryTeller <$> s .: "text"
      Just "choice"      -> Decision <$> s .: "choices"
      _                  -> fail "Wrong step type"

instance FromJSON Choice where
  parseJSON = withObject "choice" $ \c ->
    Choice
      <$> c .: "correct"
      <*> c .: "text"
      <*> c .:? "steps" .!= []

-- | Play a step in the UI monad.
playStep :: UI m => Step -> m ()
playStep (Speak character message) = tell $ character <> ": " <> message
playStep (StoryTeller message)     = tell message
playStep step@(Decision choices)   = do
    choice <- optionPanel $ zip choices $ map text choices
    playChoice choice
    unless (correct choice) $ playStep step
  where
    playChoice = mapM_ playStep . subSteps
