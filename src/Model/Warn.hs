-- |
-- Module      : Model.Warn
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- Validation utility.

module Model.Warn (Warn (..)) where

class Warn a where
  warnings :: a -> [String]
