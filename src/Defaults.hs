module Defaults where

import Control.Lens ((^.))
import Data.Default (Default
                    ,def)
import System.Random (mkStdGen)

import World (World(World), _worldWidth, _worldHeight, worldWidth, worldHeight, _worldPlayer, _worldEnemies, _worldBullets, _worldStdGen)
import Player (Player(Player), _playerPosition)

instance Default World where
  def = World { _worldWidth=100, _worldHeight=20, _worldPlayer=def, _worldEnemies=[], _worldBullets=[], _worldStdGen=mkStdGen 0 }

instance Default Player where
  def = Player { _playerPosition=(floor (fromIntegral (width - 1) / 2.0), height - 1) }
    where
      world = def :: World
      height = world ^. worldHeight
      width = world ^. worldWidth
