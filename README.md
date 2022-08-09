# CLI Game Engine

A CLI game engine that plays simple [games defined in YAML](./example/game.yml):

```sh
cabal run play -- example/game.yml
```

<p align="center">
  <img src="example/gameplay.gif">
</p>

Although games can be provided in YAML, this is a sketch of their model:

```haskell
-- A game consists of an optional introductory section and a set of stages
data Game =
  Game
    { intro  :: Maybe Section,
      stages :: Set Stage
    }

-- A stage is a sequence of sections
data Stage =
  Stage
    { sections :: [Section]
    }

-- A section is a sequence of steps with a game progress requirement
-- (i.e. dependencies from other stage sections)
data Section = Section
  { requirements :: Progress,
    steps        :: [Step]
  }

-- A step can be one of the following:
data Step
  = Speak String String -- A character telling something
  | StoryTeller String  -- The storyteller telling something
  | Decision [Choice]   -- The player has to decide among some options

-- An option can be correct or not, and it can lead to more steps
data Choice =
  Choice
    { correct  :: Bool,
      text     :: String,
      subSteps :: [Step]
    }
```
