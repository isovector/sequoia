module Game.Sequoia.Combinators
    ( focusing
    ) where

import Game.Sequoia.Geometry
import Game.Sequoia.Scene
import Game.Sequoia.Types

focusing :: Prop' a -> Prop' a -> Prop' a
focusing = move . posDif origin . center

