module Game.Sequoia.Utils
    ( mapT
    , showTrace
    ) where

import Control.Arrow ((***))
import Control.Monad (join)
import Debug.Trace

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT = join (***)

showTrace :: Show a => a -> a
showTrace = trace =<< show

