-- |
-- Module      : I18n.Applicative
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- Literal translation context within an applicative.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module I18n.Applicative (ApplicativeI18n (..)) where

import qualified I18n.Literals as L

-- | An applicative with an associated 'Language'. It allows to
-- obtain language dependant data within applicative computations.
class (Applicative a, L.Literals (L a)) => ApplicativeI18n a where
  type L a :: L.Language
  selectStage :: a String
  selectStage = pure $ L.selectStage @(L a)
  gameLoaded :: String -> a String
  gameLoaded = pure . L.gameLoaded @(L a)
  gameOver :: a String
  gameOver = pure $ L.gameOver @(L a)
  enterToContinue :: a String
  enterToContinue = pure $ L.enterToContinue @(L a)
  selectOption :: a String
  selectOption = pure $ L.selectOption @(L a)
  wrongOption :: a String
  wrongOption = pure $ L.wrongOption @(L a)
  nothingToDo :: a String
  nothingToDo = pure $ L.nothingToDo @(L a)
  blockedStage :: a String
  blockedStage = pure $ L.blockedStage @(L a)
