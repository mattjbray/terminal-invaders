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
              ,gameLoop
              ,toGameEvent)

main :: IO ()
main = do
  vty <- mkVty def
  loop vty def

loop :: Vty -> World -> IO ()
loop vty world = do
  let pic = picForImage (render world)
  update vty pic
  e <- nextEvent vty
  let (action, newWorld) = runState (gameLoop (toGameEvent e)) world
  case action of GCQuit -> shutdown vty
                 _      -> loop vty newWorld
