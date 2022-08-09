-- |
-- Module      : I18n.Literals
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- Translatable literals used in the UI for a CLI game.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}

module I18n.Literals (Language(..), Literals (..)) where

data Language = Es | En

class Literals (a :: Language) where
  selectStage :: String
  gameLoaded :: String -> String
  gameOver :: String
  enterToContinue :: String
  selectOption :: String
  wrongOption :: String
  nothingToDo :: String
  blockedStage :: String

instance Literals 'Es where
  selectStage = "Elige a donde ir:"
  gameLoaded name = "Juego cargado: " <> name <> "."
  gameOver = "Fin del juego."
  enterToContinue = "(pulsa intro para continuar)"
  selectOption = "(escribe un número de opción y pulsa intro)"
  wrongOption = "Opción no válida."
  nothingToDo = "Nada que hacer por aquí."
  blockedStage = "He de hacer algo más antes de volver aquí."

instance Literals 'En where
  selectStage = "Choose where to go:"
  gameLoaded name = "Game loaded: " <> name <> "."
  gameOver = "Game over."
  enterToContinue = "(press enter to continue)"
  selectOption = "(type an option number and press enter)"
  wrongOption = "Wrong option."
  nothingToDo = "Nothing to do here."
  blockedStage = "I must do something more before going back here."
