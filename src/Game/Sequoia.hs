{-# LANGUAGE RecordWildCards #-}

-- Strongly inspired by Helm.
-- See: http://helm-engine.org

module Game.Sequoia
    ( EngineConfig (..)
    , run
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import Foreign.C.String (withCAString)
import Game.Sequoia.Engine
import Game.Sequoia.Signal
import Game.Sequoia.Types
import Game.Sequoia.Utils
import qualified Data.Map as M
import qualified Game.Sequoia.Window as Window
import qualified Graphics.UI.SDL as SDL

data EngineConfig = EngineConfig {
  windowDimensions :: (Int, Int),
  -- windowIsFullscreen :: Bool,
  -- windowIsResizable :: Bool,
  windowTitle :: String
}

startup :: EngineConfig -> IO Engine
startup (EngineConfig { .. }) = withCAString windowTitle $ \title -> do
    let (w, h) = mapT fromIntegral windowDimensions
        wflags = foldl (.|.) 0 $
            [ SDL.windowFlagShown
            -- , SDL.windowFlagResizable  | windowIsResizable
            -- , SDL.windowFlagFullscreen | windowIsFullscreen
            ]
        rflags = SDL.rendererFlagPresentVSync .|.
                 SDL.rendererFlagAccelerated

    window   <- SDL.createWindow title 0 0 w h wflags
    renderer <- SDL.createRenderer window (-1) rflags
    return Engine { window   = window
                  , renderer = renderer
                  , cache    = M.empty
                  , continue = True
                  }

run :: EngineConfig -> Signal [Prop a] -> IO ()
run cfg scene = do
    e <- startup cfg
    mail' engineAddr e
    let app = (,) <$> scene <*> Window.dimensions
        run' i = do
            continue <- runSignal app i >>= run''
            when continue . run' $ i + 1

        run'' (s, (w, h))
    run' 0
    SDL.quit

quitRequested :: Signal Bool
quitRequested = liftIO SDL.quitRequested
