module Collisions (playerCollidedWithEnemy)

where

import Control.Lens ((^.)
                    ,anyOf
                    ,traversed)

import World (World, worldPlayer, worldEnemies)
import Player (playerPosition)
import Enemy (enemyPosition)

playerCollidedWithEnemy :: World -> Bool
playerCollidedWithEnemy world = anyOf (worldEnemies.traversed.enemyPosition) (==pPos) world
  where
    pPos = world ^. worldPlayer . playerPosition
