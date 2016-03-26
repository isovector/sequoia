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

{-# NOINLINE engine #-}
{-# NOINLINE engineAddr #-}
engine     :: Signal Engine
engineAddr :: Address Engine
(engine, engineAddr) = newMailbox "engine" $ error "undefined engine, somehow"

