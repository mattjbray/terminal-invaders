{-# LANGUAGE TemplateHaskell #-}

module World (
  World(World)
  , _worldPlayer
  , worldPlayer
  , _worldWidth
  , worldWidth
  , _worldHeight
  , worldHeight
)

where

import Control.Lens (makeLenses)
import Data.Default (Default
                    ,def)

import Player (Player)

data World = World { _worldWidth :: Int
                   , _worldHeight :: Int
                   , _worldPlayer :: Player
                   }

makeLenses ''World
