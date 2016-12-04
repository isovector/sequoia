module Game.Sequoia.Engine
    ( Engine (..)
    ) where

import Game.Sequoia.Color
import Game.Sequoia.Signal
import qualified Graphics.UI.SDL as SDL

data Engine = Engine
    { window    :: SDL.Window
    , renderer  :: SDL.Renderer
    , continue  :: Bool
    , backColor :: Color
    }

