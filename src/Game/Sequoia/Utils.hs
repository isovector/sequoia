module Game.Sequoia.Utils
    ( mapT
    ) where

import Control.Arrow ((***))
import Control.Monad (join)

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT = join (***)

