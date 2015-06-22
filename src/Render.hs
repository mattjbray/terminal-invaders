module Render (render)
where

import Control.Lens ((^.))
import qualified Data.Map as M
import Graphics.Vty (Image
                    ,char
                    ,backgroundFill
                    ,defAttr
                    ,horizCat
                    ,string
                    ,vertCat
                    ,(<|>)
                    ,(<->))

import Bullet (Bullet, bulletPosition)
import World (World, worldBullets, worldPlayer, worldWidth, worldHeight, worldEnemies, worldScore)
import Player (Player, playerPosition)
import Enemy (Enemy, enemyPosition)


class Render a where
  render :: a -> Image


instance Render World where
  render world = (topLeft <|> hEdge <|> topRight) <->
                 (vEdge <|> game <|> vEdge) <->
                 (bottomLeft <|> hEdge <|> bottomRight) <->
                 score
    where
      game = vertCat [ horizCat [ renderPixel (x,y) | x <- [0..world ^. worldWidth - 1] ] | y <- [0..world ^. worldHeight - 1] ]
      renderPixel pos = case M.lookup pos (locate world) of
                          (Just img) -> head img
                          Nothing    -> backgroundFill 1 1
      topLeft     = char defAttr '┌'
      topRight    = char defAttr '┐'
      bottomLeft  = char defAttr '└'
      bottomRight = char defAttr '┘'
      hEdge = horizCat $ replicate (world ^. worldWidth)  (char defAttr '─')
      vEdge = vertCat  $ replicate (world ^. worldHeight) (char defAttr '│')
      score = string defAttr $ "Score: " ++ show (world ^. worldScore)


-- From the World, generate a list of Images to be rendered at each position.
locate :: World -> M.Map (Int, Int) [Image]
locate world = M.alter insertPlayer (world ^. worldPlayer . playerPosition) (M.union bulletLocations enemyLocations)
  where
    insertPlayer :: Maybe [Image] -> Maybe [Image]
    insertPlayer Nothing = Just [render (world ^. worldPlayer)]
    -- TODO: Find a better way of rendering two objects in one place
    insertPlayer (Just imgs) = Just $ char defAttr 'ô' : imgs
    enemyLocations = foldl (\locations enemy -> M.insertWith (++) (enemy ^. enemyPosition) [render enemy] locations)
                           M.empty
                           (world ^. worldEnemies)
    bulletLocations = foldl (\locations bullet -> M.insertWith (++) (bullet ^. bulletPosition) [render bullet] locations)
                           M.empty
                           (world ^. worldBullets)


instance Render Player where
  render player = char defAttr '^'


instance Render Enemy where
  render enemy = char defAttr 'o'


instance Render Bullet where
  render bullet = char defAttr '.'
