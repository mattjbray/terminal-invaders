import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan
                               ,newChan
                               ,readChan
                               ,writeChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get)
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
              ,stepWorld
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


-- The main loop - render and update.
loop :: Vty -> Chan GameEvent -> StateT World IO ()
loop vty gameChan = do
  renderS vty
  action <- updateS gameChan
  case action of GCQuit -> liftIO $ shutdown vty
                 _      -> loop vty gameChan


-- Render the World to the Vty.
renderS :: Vty -> StateT World IO ()
renderS vty = do
  world <- get
  let pic = picForImage (render world)
  liftIO $ update vty pic


-- Wait on a GameEvent and update the World.
updateS :: Chan GameEvent -> StateT World IO GameControlEvent
updateS gameChan = do
  ge <- liftIO $ readChan gameChan
  hoistState $ stepWorld ge
