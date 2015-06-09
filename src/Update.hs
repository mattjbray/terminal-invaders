module Update (GameControlEvent(GCQuit)
              ,GameEvent
              ,gameLoop
              ,tick
              ,toGameEvent)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan
                               ,writeChan)
import Control.Lens ((^.)
                    ,(%~)
                    ,(%=)
                    ,_1
                    ,_2
                    ,use)
import Control.Monad (forever)
import Control.Monad.State (State)
import Graphics.Vty (Event(EvKey)
                    ,Key(KChar,KEsc,KLeft,KRight,KUp,KDown))
import System.Random (Random, random, randomR, randomIO)

import World (World, worldPlayer, worldWidth, worldHeight)
import Player (playerPosition)


data Direction = DLeft | DRight | DUp | DDown
  deriving (Bounded, Enum)

instance Random Direction where
  random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
    (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')

data GameControlEvent = GCContinue | GCQuit
data GameEvent = Quit | Continue | MovePlayer Direction


tick :: Chan GameEvent -> IO ()
tick gameChan = forever $ do
  threadDelay $ 10 ^ 5
  direction <- randomIO
  writeChan gameChan (MovePlayer direction)


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
toGameEvent _                     = Continue


movePlayer :: Direction -> State World ()
movePlayer DLeft   = worldPlayer . playerPosition . _1 %= (\x -> max (x-1) 1)
movePlayer DUp     = worldPlayer . playerPosition . _2 %= (\y -> max (y-1) 1)
movePlayer DRight  = do
  maxWidth <- use worldWidth
  worldPlayer . playerPosition . _1 %= (\x -> min (x+1) maxWidth)
movePlayer DDown   = do
  maxHeight <- use worldHeight
  worldPlayer . playerPosition . _2 %= (\y -> min (y+1) maxHeight)

gameLoop :: GameEvent -> State World GameControlEvent
gameLoop Quit           = return GCQuit
gameLoop Continue       = return GCContinue
gameLoop (MovePlayer d) = movePlayer d >> return GCContinue
