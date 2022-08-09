-- |
-- Module      : Platform.GameStation
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- A monad to play CLI games.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Platform.GameStation (GameStation, evalGameStation) where

import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT))
import           Control.Monad.State.Strict (MonadIO, MonadState, StateT,
                                             evalStateT)
import           I18n.Applicative           (ApplicativeI18n (..))
import           I18n.Literals              (Language, Literals)
import           Model.Game                 (Game, Progress, initialProgress)
import           UI.IOConfig                (withInteractionDisabled)

-- | Game monad stack. Reader for the game ROM, state for
-- the player progress and IO for the UI.
newtype GameStationStack (lang :: Language) a =
  GameStation
    { gameStation :: StateT Progress (ReaderT Game IO) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Progress
    , MonadReader Game
    )

-- | Give translation access to a GameStationStack by
-- using its phantom type parameter lang.
instance Literals lang => ApplicativeI18n (GameStationStack lang) where
  type L (GameStationStack lang) = lang

-- | GameStationStack with its lang type parameter hidden.
type GameStation a =
  forall lang. (Literals lang) => GameStationStack lang a

-- | Evaluate a GameStack computation with a loaded Game ROM.
-- This function requires the lang type to be applied.
evalGameStation :: Literals lang => GameStationStack lang a -> Game -> IO a
evalGameStation GameStation {gameStation = process} =
  withInteractionDisabled . runReaderT (evalStateT process initialProgress)
