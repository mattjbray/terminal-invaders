{-# LANGUAGE TemplateHaskell #-}

module Player (
  Player(Player)
  , playerPosition
  , _playerPosition
)
where

import Control.Lens (makeLenses)

data Player = Player { _playerPosition :: (Int, Int) }

makeLenses ''Player
