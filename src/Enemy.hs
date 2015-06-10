{-# LANGUAGE TemplateHaskell #-}

module Enemy (
  Enemy(Enemy)
  , enemyPosition
  , _enemyPosition
)
where

import Control.Lens (makeLenses)

data Enemy = Enemy { _enemyPosition :: (Int, Int) }

makeLenses ''Enemy
