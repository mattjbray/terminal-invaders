module Defaults where

import Control.Lens ((^.))
import Data.Default (Default
                    ,def)

import World (World(World), _worldWidth, _worldHeight, worldWidth, worldHeight, _worldPlayer)
import Player (Player(Player), _playerPosition)

instance Default World where
  def = World { _worldWidth=100, _worldHeight=20, _worldPlayer=def }

instance Default Player where
  def = Player { _playerPosition=(floor (fromIntegral width / 2.0), height) }
    where
      world = def :: World
      height = world ^. worldHeight
      width = world ^. worldWidth
