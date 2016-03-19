module Game.Sequoia.Engine
    ( Engine (..)
    , engine
    , engineAddr
    ) where

import Game.Sequoia.Signal
import System.IO.Unsafe (unsafePerformIO)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Data.Map as Map

data Engine = Engine
    { window   :: SDL.Window
    , renderer :: SDL.Renderer
    , cache    :: Map.Map FilePath Cairo.Surface
    , continue :: Bool
    }

engine     :: Signal Engine
engineAddr :: Address Engine
(engine, engineAddr) = unsafePerformIO $ mailbox undefined

