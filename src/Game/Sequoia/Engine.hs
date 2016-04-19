module Game.Sequoia.Engine
    ( Engine (..)
    , poll
    ) where

import Game.Sequoia.Signal
import qualified Graphics.UI.SDL as SDL

data Engine = Engine
    { window   :: SDL.Window
    , renderer :: SDL.Renderer
    , continue :: Bool
    }

-- TODO(sandy): this seems to sample unreasonably fast
poll :: IO a -> Now (Behavior a)
poll io = loop
  where
    loop = do
        e  <- async (return ())
        e' <- planNow $ loop <$ e
        a <- sync io
        return $ pure a `switch` e'
