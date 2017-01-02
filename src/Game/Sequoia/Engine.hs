module Game.Sequoia.Engine
    ( Engine (..)
    ) where

import           Data.IORef (IORef)
import           Data.Map (Map)
import           Game.Sequoia.Color
import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL.Raw as SDL

data Engine = Engine
    { window    :: SDL.Window
    , renderer  :: SDL.Renderer
    , continue  :: Bool
    , backColor :: Color
    , cache     :: IORef (Map FilePath Cairo.Surface)
    }

