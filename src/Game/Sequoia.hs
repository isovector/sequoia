{-# LANGUAGE RecordWildCards #-}

-- Strongly inspired by Helm.
-- See: http://helm-engine.org

module Game.Sequoia
    ( EngineConfig (..)
    , run
    , mailing
    , amnesia
    , module Control.Applicative
    , module Game.Sequoia.Geometry
    , module Game.Sequoia.Scene
    , module Game.Sequoia.Signal
    , module Game.Sequoia.Time
    , module Game.Sequoia.Types
    ) where

import Control.Applicative
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Data.Bits ((.|.))
import Data.SG.Shape
import Foreign.C.String (withCAString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Storable (peek)
import Game.Sequoia.Engine
import Game.Sequoia.Geometry
import Game.Sequoia.Scene
import Game.Sequoia.Signal
import Game.Sequoia.Time
import Game.Sequoia.Types
import Game.Sequoia.Utils
import System.Endian (fromBE32)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import qualified Game.Sequoia.Window as Window
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.SDL as SDL

globalTime :: IORef Int
globalTime = unsafePerformIO $ newIORef 0

data EngineConfig = EngineConfig {
  windowDimensions :: (Int, Int),
  -- windowIsFullscreen :: Bool,
  -- windowIsResizable :: Bool,
  windowTitle :: String
}

{-# NOINLINE mailing #-}
mailing :: Address a -> (a -> a) -> b -> b
mailing addr f b = unsafePerformIO $ do
    now <- readIORef globalTime
    sampleAt now $ mail addr f
    return b

{-# NOINLINE amnesia #-}
-- |A signal which remembers only one sample in the past. Highly unsafe to
-- reminisce about, as you might imagine.
amnesia :: a -> Signal a -> Signal a
amnesia a sa = unsafePerformIO $ do
    current <- newIORef a
    last    <- newIORef a
    sample  <- newIORef 0

    return . Signal $ \i -> do
        now <- readIORef globalTime
        ago <- readIORef sample
        when (now /= ago) $ do
            readIORef current >>= writeIORef last
            runSignal sa now >>=  writeIORef current
            writeIORef sample now
        case now - i of
          0 -> readIORef current
          1 -> readIORef last
          _ -> return a

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
    mail' engineAddr $ const e
    let app = (,) <$> scene <*> Window.dimensions
        run' i = do
            writeIORef globalTime i
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

    Cairo.save
    uncurry Cairo.translate $ mapT ((/ 2) . fromIntegral) size
    mapM_ renderProp ps
    Cairo.restore

renderProp :: Prop' a -> Cairo.Render ()
renderProp (GroupProp f)   = mapM_ renderProp f
renderProp (ShapeProp _ f) = renderForm f
renderProp (BakedProp _ f) = mapM_ renderForm f

renderForm :: Form -> Cairo.Render ()
renderForm (Form (Style mfs mls) s) = do
    Cairo.newPath
    case s of
      Rectangle { .. } -> do
          let (w, h) = mapT (*2) rectSize
              (x, y) = unpackPos shapeCentre
          Cairo.rectangle (x - w/2) (y - h/2) w h

      Polygon { .. } -> do
          forM_ polyPoints $ \rel -> do
              let pos = plusDir shapeCentre rel
              unpackFor pos Cairo.lineTo
          Cairo.closePath

      Circle { .. } -> do
          unpackFor shapeCentre Cairo.arc circSize 0 (pi * 2)
    mapM_ setFillStyle mfs
    mapM_ setLineStyle mls

setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle (LineStyle { .. }) = do
    unpackColFor lineColor Cairo.setSourceRGBA
    setLineCap
    setLineJoin
    Cairo.setLineWidth lineWidth
    Cairo.setDash lineDashing lineDashOffset
    Cairo.strokePreserve
  where
    setLineCap = Cairo.setLineCap $
        case lineCap of
          FlatCap   -> Cairo.LineCapButt
          RoundCap  -> Cairo.LineCapRound
          PaddedCap -> Cairo.LineCapSquare
    setLineJoin =
        case lineJoin of
          SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
          SharpJoin lim -> Cairo.setLineJoin Cairo.LineJoinMiter
                        >> Cairo.setMiterLimit lim
          ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel


setFillStyle :: FillStyle -> Cairo.Render ()
setFillStyle (Solid col) = do
    unpackColFor col Cairo.setSourceRGBA
    Cairo.fillPreserve

unpackFor :: Pos -> (Double -> Double -> a) -> a
unpackFor p f = uncurry f $ unpackPos p

unpackColFor :: Color
             -> (Double -> Double ->  Double -> Double -> a)
             -> a
unpackColFor (Color r g b a) f = f r g b a

