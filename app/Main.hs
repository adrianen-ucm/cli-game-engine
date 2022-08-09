{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           I18n.Literals        (Language (..))
import           Model.Game           (fromYamlFileThrow, warnings)
import           Platform.GameStation (evalGameStation)
import           Platform.Process     (playGame)
import           System.Environment   (getArgs)

-- | A simple example of loading a game from a file,
-- checking if it has some warning messages and playing
-- it with a given language for the UI.
main :: IO ()
main = do
  gameFile <- head <$> getArgs          -- Throws if no argument has been provided
  gameROM <- fromYamlFileThrow gameFile -- Throws if something goes wrong
  case warnings gameROM of
    [] -> evalGameStation @'En playGame gameROM
    es -> putStrLn $ unlines es
