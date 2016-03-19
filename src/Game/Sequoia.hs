{-# LANGUAGE RecordWildCards #-}

-- Strongly inspired by Helm.
-- See: http://helm-engine.org

module Game.Sequoia
    ( EngineConfig (..)
    , run
    , module Control.Applicative
    , module Game.Sequoia.Scene
    , module Game.Sequoia.Signal
    , module Game.Sequoia.Time
    , module Game.Sequoia.Types
    ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Data.Bits ((.|.))
import Data.SG.Shape
import Foreign.C.String (withCAString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Storable (peek)
import Game.Sequoia.Engine
import Game.Sequoia.Scene
import Game.Sequoia.Signal
import Game.Sequoia.Time
import Game.Sequoia.Types
import Game.Sequoia.Utils
import System.Endian (fromBE32)
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
                  , continue = True
                  }

run :: EngineConfig -> Signal [Prop' a] -> IO ()
run cfg scene = do
    e <- startup cfg
    mail' engineAddr e
    let app = (,) <$> scene <*> Window.dimensions
        run' i = do
            continue <- runSignal app i >>= run''
            when continue . run' $ i + 1

        run'' = uncurry $ render e
    run' 0
    SDL.quit

render :: Engine -> [Prop' a] -> (Int, Int) -> IO Bool
render e@(Engine { .. }) ps size@(w, h) =
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
        Cairo.withImageSurfaceForData
            (castPtr pixels)
            Cairo.FormatARGB32
            (fromIntegral w)
            (fromIntegral h)
            pitch
            $ \surface ->
                Cairo.renderWith surface $ render' ps size
        SDL.unlockTexture texture
        SDL.renderClear renderer
        SDL.renderCopy renderer texture nullPtr nullPtr
        SDL.destroyTexture texture
        SDL.renderPresent renderer

        not <$> SDL.quitRequested

render' :: [Prop' a] -> (Int, Int) -> Cairo.Render ()
render' ps size = do
    Cairo.setSourceRGB 0 0 0
    uncurry (Cairo.rectangle 0 0) $ mapT fromIntegral size
    Cairo.fill

    mapM_ renderProp ps
    return ()

renderProp :: Prop' a -> Cairo.Render ()
renderProp (GroupProp f)   = mapM_ renderProp f
renderProp (ShapeProp _ f) = renderForm f
renderProp (BakedProp _ f) = mapM_ renderForm f

renderForm :: Form -> Cairo.Render ()
renderForm (Form fs s) = do
    case s of
      Rectangle { .. } -> do
          let (w, h) = rectSize
          unpackFor shapeCentre Cairo.rectangle w h

    setFillStyle fs

setFillStyle :: FillStyle -> Cairo.Render ()
setFillStyle (Solid (Color r g b a)) = do
    Cairo.setSourceRGBA r g b a
    Cairo.fill

unpackFor :: Pos -> (Double -> Double -> a) -> a
unpackFor p f = uncurry f $ unpackPos p

