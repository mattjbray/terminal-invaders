{-# LANGUAGE TemplateHaskell #-}

module Bullet (
  Bullet(Bullet)
  , bulletPosition
  , bulletVel
)

where

import Control.Lens (makeLenses)

data Bullet = Bullet { _bulletPosition :: (Int, Int)
                     , _bulletVel :: (Int, Int)
                     }

makeLenses ''Bullet
