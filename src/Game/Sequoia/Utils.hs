module Game.Sequoia.Utils
    ( mapT
    , safeHead
    , showTrace
    ) where

import Control.Arrow ((***))
import Control.Monad (join)
import Debug.Trace

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT = join (***)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

showTrace :: Show a => a -> a
showTrace = trace =<< show

