{-# LANGUAGE TemplateHaskell #-}

module World (
  World(World)
  , _worldPlayer
  , worldPlayer
  , _worldWidth
  , worldWidth
  , _worldHeight
  , worldHeight
  , _worldEnemies
  , worldEnemies
)

where

import Control.Lens (makeLenses)

import Enemy (Enemy)
import Player (Player)

data World = World { _worldWidth :: Int
                   , _worldHeight :: Int
                   , _worldPlayer :: Player
                   , _worldEnemies :: [Enemy]
                   }

makeLenses ''World
