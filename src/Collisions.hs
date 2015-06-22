module Collisions (collided)

where

import Control.Lens ((^.)
                    ,anyOf
                    ,traversed)

import World (World, worldPlayer, worldEnemies)
import Player (playerPosition)
import Enemy (enemyPosition)

collided :: World -> Bool
collided world = anyOf (worldEnemies.traversed.enemyPosition) (==pPos) world
  where
    pPos = world ^. worldPlayer . playerPosition
