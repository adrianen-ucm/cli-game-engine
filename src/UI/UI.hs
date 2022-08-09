-- |
-- Module      : UI.UI
-- Copyright   : (c) Adrián Enríquez Ballester, 2022
--
-- UI components for the CLI game.

{-# LANGUAGE ConstraintKinds #-}

module UI.UI
  ( UI,
    tell,
    confirm,
    option,
    optionPanel
  )
where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           I18n.Applicative       (ApplicativeI18n (..))
import           UI.IO                  (clearScreen, getLineInt, slowPutStrLn)
import           UI.IOConfig            (withInteractionEnabled)

-- | Type alias for the UI monad constraint.
type UI m = (ApplicativeI18n m, MonadIO m)

-- | Show text interactively.
tell :: UI m => String -> m ()
tell message = do
  liftIO clearScreen
  liftIO $ slowPutStrLn message
  confirm

-- | Confirmation message.
confirm :: UI m => m ()
confirm = do
  enterToContinue >>= liftIO . putStrLn
  _ <- liftIO getChar
  pure ()

-- | Pick an option.
option :: UI m => [(a, String)] -> String -> m a
option options title = do
  liftIO clearScreen
  liftIO $ slowPutStrLn title
  optionPanel options

-- | Pick an option panel.
optionPanel :: UI m => [(a, String)] -> m a
optionPanel options = do
    liftIO $ putStrLn $ ('\n' :) . unlines $ map prettyOption enumerated
    selectOption >>= liftIO . putStrLn
    inputLoop
  where
    enumerated = zip [1 ..] options
    prettyOption (n, (_, label)) = show n <> ") " <> label
    inputLoop = do
      selected <- liftIO $ withInteractionEnabled getLineInt
      case selected >>= (`lookup` enumerated) of
        Just (o, _) -> pure o
        Nothing     -> do
          wrongOption >>= liftIO . putStrLn
          inputLoop
