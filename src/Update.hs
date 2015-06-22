module Update (GameControlEvent(GCQuit)
              ,GameEvent
              ,stepWorld
              ,tick
              ,toGameEvent)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan
                               ,writeChan)
import Control.Lens ((%=)
                    ,(+=)
                    ,(*=)
                    ,(^.)
                    ,(.=)
                    ,_1
                    ,_2
                    ,traversed
                    ,view
                    ,zoom
                    ,use)
import Control.Monad (forever,liftM,when)
import Control.Monad.State (State, get, gets)
import qualified Data.Set as Set
import Graphics.Vty (Event(EvKey)
                    ,Key(KChar,KEsc,KLeft,KRight,KUp,KDown))
import System.Random (Random, random, randomR)

import World (World, inWorld, worldPlayer, worldWidth, worldHeight, worldEnemies, worldBullets, worldStdGen, worldScore)
import Bullet (Bullet(Bullet), bulletVel, bulletPosition)
import Player (playerPosition)
import Enemy (Enemy(Enemy),enemyPosition, enemyVel)
import Collisions (playerCollidedWithEnemy)


data Direction = DLeft | DRight | DUp | DDown
  deriving (Bounded, Enum)

instance Random Direction where
  random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
    (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')

data GameControlEvent = GCContinue | GCQuit
data GameEvent = Quit | Tick | Pass | MovePlayer Direction


tick :: Chan GameEvent -> IO ()
tick gameChan = forever $ do
  threadDelay $ 10 ^ 5
  writeChan gameChan Tick


toGameEvent :: Event -> GameEvent
toGameEvent (EvKey KEsc        _) = Quit
toGameEvent (EvKey (KChar 'q') _) = Quit
toGameEvent (EvKey KLeft       _) = MovePlayer DLeft
toGameEvent (EvKey (KChar 'h') _) = MovePlayer DLeft
toGameEvent (EvKey KRight      _) = MovePlayer DRight
toGameEvent (EvKey (KChar 'l') _) = MovePlayer DRight
toGameEvent (EvKey KUp         _) = MovePlayer DUp
toGameEvent (EvKey (KChar 'k') _) = MovePlayer DUp
toGameEvent (EvKey KDown       _) = MovePlayer DDown
toGameEvent (EvKey (KChar 'j') _) = MovePlayer DDown
toGameEvent _                     = Pass


movePlayer :: Direction -> State World ()
movePlayer DLeft   = worldPlayer . playerPosition . _1 %= (\x -> max (x-1) 0)
movePlayer DUp     = worldPlayer . playerPosition . _2 %= (\y -> max (y-1) 0)
movePlayer DRight  = do
  maxWidth <- use worldWidth
  worldPlayer . playerPosition . _1 %= (\x -> min (x+1) (maxWidth - 1))
movePlayer DDown   = do
  maxHeight <- use worldHeight
  worldPlayer . playerPosition . _2 %= (\y -> min (y+1) (maxHeight - 1))


stepWorld :: GameEvent -> State World GameControlEvent
stepWorld Quit           = return GCQuit
stepWorld Pass           = return GCContinue
stepWorld Tick           = do moveBullets
                              checkCollisions
                              moveEnemies
                              checkCollisions
                              spawnEnemy
                              checkCollisions
                              spawnBullets
                              checkCollisions
stepWorld (MovePlayer d) = movePlayer d >> checkCollisions


checkCollisions :: State World GameControlEvent
checkCollisions = do
  -- remove any enemies and bullets that have collided
  bulletsBefore <- liftM length $ use worldBullets
  bPositions <- liftM (Set.fromList . map (view bulletPosition)) $ use worldBullets
  ePositions <- liftM (Set.fromList . map (view enemyPosition))  $ use worldEnemies
  worldEnemies %= (\es -> [e | e <- es, (e^.enemyPosition)  `Set.notMember` bPositions])
  worldBullets %= (\bs -> [b | b <- bs, (b^.bulletPosition) `Set.notMember` ePositions])
  bulletsAfter <- liftM length $ use worldBullets
  worldScore += (bulletsBefore - bulletsAfter)
  -- quit if the player met an enemy
  gets (\w -> if playerCollidedWithEnemy w then GCQuit else GCContinue)


spawnBullets :: State World ()
spawnBullets = do
  score <- use worldScore
  (x,y) <- use (worldPlayer.playerPosition)
  worldBullets %= (:) (Bullet (x,y-1) (0,-1))
  when (score > 10) $ do
    worldBullets %= (:) (Bullet (x-1,y-1) (-1,-1))
    worldBullets %= (:) (Bullet (x+1,y-1) (1,-1))

spawnEnemy :: State World ()
spawnEnemy = do
  g <- use worldStdGen
  let (p, g') = randomR (0 :: Int, 100) g
  worldStdGen .= g'
  when (p>85) $ do
    width <- use worldWidth
    let (x, g'') = randomR (0, width) g'
    worldStdGen .= g''
    worldEnemies %= (:) (Enemy (x, 0) (0,1))

moveBullets :: State World ()
moveBullets = do
  height <- use worldHeight
  width <- use worldWidth
  zoom (worldBullets.traversed) $ do
    (dx,dy) <- use bulletVel
    bulletPosition %= (\(x,y) -> (x+dx, y+dy))
  world <- get
  worldBullets %= (\bs -> [b | b <- bs, inWorld world (b ^. bulletPosition)])


moveEnemies :: State World ()
moveEnemies = do
  height <- use worldHeight
  width <- use worldWidth
  zoom (worldEnemies.traversed) $ do
    bounceEnemy width height
    (dx,dy) <- use enemyVel
    enemyPosition %= (\(x,y) -> (x+dx, y+dy))


bounceEnemy :: Int -> Int -> State Enemy ()
bounceEnemy width height = do
    (dx,dy) <- use enemyVel
    (x,y)   <- use enemyPosition
    let (testX, testY) = (x+dx,y+dy)
    when (testX < 0 || testX >= width)  $ enemyVel . _1 *= -1
    when (testY < 0 || testY >= height) $ enemyVel . _2 *= -1
