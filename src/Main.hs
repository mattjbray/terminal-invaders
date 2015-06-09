import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan
                               ,newChan
                               ,readChan
                               ,writeChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, StateT, evalStateT, get)
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
import Utils (hoistState)

main :: IO ()
main = do
  vty <- mkVty def
  gameChan <- newChan
  -- thread to tick the game
  forkIO (tick gameChan)
  -- thread to wait on Vty events
  forkIO (inputToGamechan vty gameChan)
  evalStateT (loop vty gameChan) def


-- map Vty input events to GameEvents on the game channel
inputToGamechan :: Vty -> Chan GameEvent -> IO ()
inputToGamechan vty gameChan = forever $ do
  e <- nextEvent vty
  writeChan gameChan (toGameEvent e)


loop :: Vty -> Chan GameEvent -> StateT World IO ()
loop vty gameChan = do
  world <- get
  let pic = picForImage (render world)
  liftIO $ update vty pic
  ge <- liftIO $ readChan gameChan
  action <- hoistState $ gameLoop ge
  case action of GCQuit -> liftIO $ shutdown vty
                 _      -> loop vty gameChan
