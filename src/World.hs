{-# LANGUAGE TemplateHaskell #-}

module World (
  World(World)
  , inWorld
  , _worldStdGen
  , worldStdGen
  , _worldBullets
  , worldBullets
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

import Control.Lens ((^.), makeLenses)
import System.Random (StdGen)

import Bullet (Bullet)
import Enemy (Enemy)
import Player (Player)

data World = World { _worldWidth :: Int
                   , _worldHeight :: Int
                   , _worldPlayer :: Player
                   , _worldEnemies :: [Enemy]
                   , _worldBullets :: [Bullet]
                   , _worldStdGen :: StdGen
                   }

makeLenses ''World


inWorld :: World -> (Int, Int) -> Bool
inWorld world (x, y) = x >= 0 && x < world ^. worldWidth && y >= 0 && y < world ^. worldHeight
