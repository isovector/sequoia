module Game.Sequoia.Engine
    ( Engine (..)
    , engine
    , engineAddr
    ) where

import Game.Sequoia.Signal
import System.IO.Unsafe (unsafePerformIO)

import qualified Graphics.UI.SDL as SDL

data Engine = Engine
    { window   :: SDL.Window
    , renderer :: SDL.Renderer
    , continue :: Bool
    }

engine     :: Signal Engine
engineAddr :: Address Engine
(engine, engineAddr) = unsafePerformIO
                     . mailbox
                     $ error "undefined engine, somehow =("

