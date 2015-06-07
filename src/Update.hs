module Update (GameControlEvent(GCQuit)
              ,gameLoop
              ,toGameEvent)
where

import Control.Lens ((^.)
                    ,(%~)
                    ,(%=)
                    ,_1
                    ,_2
                    ,use)
import Control.Monad.State (State)
import Graphics.Vty (Event(EvKey)
                    ,Key(KChar,KEsc,KLeft,KRight,KUp,KDown))

import World (World, worldPlayer, worldWidth, worldHeight)
import Player (playerPosition)


data Direction = DLeft | DRight | DUp | DDown
data GameControlEvent = GCContinue | GCQuit
data GameEvent = Quit | Continue | MovePlayer Direction

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
