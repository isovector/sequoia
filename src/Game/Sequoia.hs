{-# LANGUAGE RecordWildCards #-}

-- Strongly inspired by Helm.
-- See: http://helm-engine.org

module Game.Sequoia
    ( EngineConfig (..)
    , run
    ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.Endian (fromBE32)
import Control.Monad (when)
import Control.Monad.State (evalStateT)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import Foreign.C.String (withCAString)
import Foreign.Ptr (nullPtr, castPtr)
import Game.Sequoia.Engine
import Game.Sequoia.Signal
import Game.Sequoia.Types
import Game.Sequoia.Utils
import qualified Data.Map as M
import qualified Game.Sequoia.Window as Window
import qualified Graphics.Rendering.Cairo as Cairo
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

        run'' (s, (w, h)) = undefined
    run' 0
    SDL.quit

render :: Engine -> [Prop a] -> (Int, Int) -> IO Engine
render e@(Engine { .. }) ps (w, h) =
    alloca $ \pixelsptr ->
    alloca $ \pitchptr  -> do
        format <- SDL.masksToPixelFormatEnum 32 (fromBE32 0x0000ff00)
                                                (fromBE32 0x00ff0000)
                                                (fromBE32 0xff000000)
                                                (fromBE32 0x000000ff)
        texture <-
            SDL.createTexture
                renderer
                format
                SDL.textureAccessStreaming
                (fromIntegral w)
                (fromIntegral h)
        SDL.lockTexture texture nullPtr pixelsptr pitchptr
        pixels <- peek pixelsptr
        pitch  <- fromIntegral <$> peek pitchptr
        res    <-
            Cairo.withImageSurfaceForData
                (castPtr pixels)
                Cairo.FormatARGB32
                (fromIntegral w)
                (fromIntegral h)
                pitch $ \surface ->
                    Cairo.renderWith surface .
                        flip evalStateT e $ render' w h ps
        SDL.unlockTexture texture
        SDL.renderClear renderer
        SDL.renderCopy renderer texture nullPtr nullPtr
        SDL.destroyTexture texture
        SDL.renderPresent renderer
        return res

render' = undefined


quitRequested :: Signal Bool
quitRequested = liftIO SDL.quitRequested

