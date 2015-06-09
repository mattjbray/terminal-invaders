module Render (render)
where

import Control.Lens ((^.))
import Graphics.Vty (Image
                    ,char
                    ,backgroundFill
                    ,defAttr
                    ,horizCat
                    ,vertCat
                    ,(<|>)
                    ,(<->))

import World (World, worldPlayer, worldWidth, worldHeight)
import Player (Player, playerPosition)

class Render a where
  render :: a -> Image

instance Render World where
  render world = (topLeft <|> hEdge <|> topRight) <->
                 (vEdge <|> game <|> vEdge) <->
                 (bottomLeft <|> hEdge <|> bottomRight)
    where
      game = let (x,y) = world ^. worldPlayer . playerPosition in
               backgroundFill (x - 1) 1 <|>
               (backgroundFill 1 (y - 1) <-> render (world ^. worldPlayer)) <|>
               backgroundFill (world ^. worldWidth - x) 1
      topLeft     = char defAttr '┌'
      topRight    = char defAttr '┐'
      bottomLeft  = char defAttr '└'
      bottomRight = char defAttr '┘'
      hEdge = horizCat $ replicate (world ^. worldWidth)  (char defAttr '─')
      vEdge = vertCat  $ replicate (world ^. worldHeight) (char defAttr '│')

instance Render Player where
  render player = char defAttr '^'
