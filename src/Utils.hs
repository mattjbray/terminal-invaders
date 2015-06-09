module Utils (hoistState)

where

import Control.Monad.State (State, StateT(StateT), runState)


-- Combine states over different monads
-- http://stackoverflow.com/a/17325795/568777
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState
