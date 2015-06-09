import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan
                               ,newChan
                               ,readChan
                               ,writeChan)
import Control.Monad (forever)
import Control.Monad.State (runState)
import Data.Default (def)
import Graphics.Vty (Vty
                    ,mkVty
                    ,nextEvent
                    ,picForImage
                    ,shutdown
                    ,update)

import World (World)
import Render (render)
import Defaults ()
import Update (GameControlEvent(GCQuit)
              ,GameEvent
              ,gameLoop
              ,tick
              ,toGameEvent)

main :: IO ()
main = do
  vty <- mkVty def
  gameChan <- newChan
  -- thread to tick the game
  forkIO (tick gameChan)
  -- thread to wait on Vty events
  forkIO (inputToGamechan vty gameChan)
  loop vty gameChan def


-- map Vty input events to GameEvents on the game channel
inputToGamechan :: Vty -> Chan GameEvent -> IO ()
inputToGamechan vty gameChan = forever $ do
  e <- nextEvent vty
  writeChan gameChan (toGameEvent e)


loop :: Vty -> Chan GameEvent -> World -> IO ()
loop vty gameChan world = do
  let pic = picForImage (render world)
  update vty pic
  ge <- readChan gameChan
  let (action, newWorld) = runState (gameLoop ge) world
  case action of GCQuit -> shutdown vty
                 _      -> loop vty gameChan newWorld
