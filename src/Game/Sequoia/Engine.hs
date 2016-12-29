module Game.Sequoia.Engine
    ( Engine (..)
    ) where

import Game.Sequoia.Color
import qualified SDL.Raw as SDL

data Engine = Engine
    { window    :: SDL.Window
    , renderer  :: SDL.Renderer
    , continue  :: Bool
    , backColor :: Color
    }

