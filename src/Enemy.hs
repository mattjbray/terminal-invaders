{-# LANGUAGE TemplateHaskell #-}

module Enemy (
  Enemy(Enemy)
  , enemyPosition
  , _enemyPosition
  , enemyVel
  , _enemyVel
)
where

import Control.Lens (makeLenses)

data Enemy = Enemy { _enemyPosition :: (Int, Int)
                   , _enemyVel :: (Int, Int) }

makeLenses ''Enemy
