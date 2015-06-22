module Defaults where

import Control.Lens ((^.))
import Data.Default (Default
                    ,def)

import Enemy (Enemy(Enemy))
import World (World(World), _worldWidth, _worldHeight, worldWidth, worldHeight, _worldPlayer, _worldEnemies, _worldBullets)
import Player (Player(Player), _playerPosition)

instance Default World where
  def = World { _worldWidth=100, _worldHeight=20, _worldPlayer=def, _worldEnemies=enemies, _worldBullets=[] }
    where
      enemies = [Enemy (x * 19, x * 2) (1, if x `mod` 2 == 0 then 1 else -1) | x <- [0..6]]

instance Default Player where
  def = Player { _playerPosition=(floor (fromIntegral (width - 1) / 2.0), height - 1) }
    where
      world = def :: World
      height = world ^. worldHeight
      width = world ^. worldWidth
