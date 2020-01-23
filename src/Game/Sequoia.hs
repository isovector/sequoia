{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE ViewPatterns                #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- Strongly inspired by Helm.
-- See: http://helm-engine.org

module Game.Sequoia
    ( EngineConfig (..)
    , module Game.Sequoia
    , module Control.Applicative
    , module Game.Sequoia.Graphics
    , module Game.Sequoia.Signal
    , module Game.Sequoia.Time
    , module Game.Sequoia.Types
    , Engine ()
    , Color ()
    , rgb
    , rgba
    ) where

import           Control.Applicative
import           Control.Exception hiding (mask)
import           Control.Monad (forM_, when)
import           Data.Array.MArray
import           Data.Bits ((.|.), (.&.), shift)
import           Data.Foldable (for_)
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Word (Word32)
import           Foreign.C.String (withCAString)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (nullPtr, castPtr)
import           Foreign.Storable (peek)
import           Game.Sequoia.Color (Color (..), rgb, rgba)
import           Game.Sequoia.Engine
import           Game.Sequoia.Graphics
import           Game.Sequoia.Signal
import           Game.Sequoia.Time
import           Game.Sequoia.Types
import           Game.Sequoia.Utils
import           Game.Sequoia.Window
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango
import qualified SDL.Raw as SDL
import           System.Endian (fromBE32)
import           System.FilePath.Posix (normalise)


data EngineConfig = EngineConfig
  { windowDimensions :: (Int, Int)
  -- windowIsFullscreen :: Bool,
  -- windowIsResizable :: Bool,
  , windowTitle :: String
  , windowColor :: Color
  }

startup :: EngineConfig -> IO Engine
startup (EngineConfig { .. }) = withCAString windowTitle $ \title -> do
    let (w, h) = mapT fromIntegral windowDimensions
        wflags = foldl (.|.) 0 $
            [ SDL.SDL_WINDOW_SHOWN
            -- , SDL.SDL_WINDOW_RESIZABLE  | windowIsResizable
            -- , SDL.SDL_WINDOW_FULLSCREEN | windowIsFullscreen
            ]
        rflags = SDL.SDL_RENDERER_PRESENTVSYNC .|.
                 SDL.SDL_RENDERER_ACCELERATED

    SDL.init SDL.SDL_INIT_EVENTS
    window      <- SDL.createWindow title 0 0 w h wflags
    renderer    <- SDL.createRenderer window (-1) rflags
    cache       <- newIORef mempty
    textureSize <- newIORef windowDimensions
    bufferT     <- makeTexture renderer windowDimensions
    buffer      <- newIORef bufferT
    return Engine { window      = window
                  , renderer    = renderer
                  , continue    = True
                  , backColor   = windowColor
                  , cache       = cache
                  , textureSize = textureSize
                  , buffer      = buffer
                  }

play :: EngineConfig
     -> (Engine -> N i)
     -> (i -> N (B Element))
     -> IO ()
play cfg initial sceneN = do
    engine <- runNowMaster $ do
        engine   <- sync $ startup cfg
        sceneSig <- initial engine >>= sceneN
        dimSig   <- getDimensions engine
        quit     <- poll $ wantsQuit engine sceneSig dimSig
        fmap (engine <$) $ sample $ whenE quit
    quitGracefully engine

wantsQuit :: Engine -> B Element -> B (Int, Int) -> N Bool
wantsQuit engine sceneSig dimSig = do
    scene <- sample sceneSig
    dims  <- sample dimSig
    sync $ do
        onException (render engine scene dims) $ quitGracefully engine
        SDL.quitRequested


quitGracefully :: Engine -> IO ()
quitGracefully engine = do
  let w = window engine
  SDL.closeAudio
  SDL.hideWindow w
  SDL.destroyWindow w
  SDL.quit

makeTexture :: SDL.Renderer -> (Int, Int) -> IO SDL.Texture
makeTexture renderer (w, h) = do
  format <- SDL.masksToPixelFormatEnum 32 (fromBE32 0x0000ff00)
                                          (fromBE32 0x00ff0000)
                                          (fromBE32 0xff000000)
                                          (fromBE32 0x000000ff)
  SDL.createTexture renderer
                    format
                    SDL.SDL_TEXTUREACCESS_STREAMING
                    (fromIntegral w)
                    (fromIntegral h)

rebuildTexture :: Engine -> (Int, Int) -> IO ()
rebuildTexture Engine {..} size = do
  curSize <- readIORef textureSize
  when (curSize /= size) $ do
    bufferT <- readIORef buffer
    SDL.destroyTexture bufferT
    bufferT' <- makeTexture renderer size
    writeIORef buffer bufferT'


render :: Engine -> Element -> (Int, Int) -> IO ()
render e@(Engine { .. }) ps size@(w, h) =
    alloca $ \pixelsptr ->
    alloca $ \pitchptr  -> do
        rebuildTexture e size
        texture <- readIORef buffer
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
                Cairo.renderWith surface $ do
                  unpackColFor backColor Cairo.setSourceRGBA
                  uncurry (Cairo.rectangle 0 0) $ mapT fromIntegral size
                  Cairo.fill
                  renderElement e ps
        SDL.unlockTexture texture
        SDL.renderClear renderer
        SDL.renderCopy renderer texture nullPtr nullPtr
        SDL.renderPresent renderer


createMask :: FilePath -> IO (Cairo.Surface, Int, Int)
createMask src = do
  surface <- Cairo.imageSurfaceCreateFromPNG src
  w <- Cairo.imageSurfaceGetWidth surface
  h <- Cairo.imageSurfaceGetHeight surface

  pixels <- Cairo.imageSurfaceGetPixels surface

  assocs <- getAssocs pixels
  forM_ assocs $ \(i :: Int, pix :: Word32) -> do
    let alpha = shift pix (-24) .&. 255
        red   = shift pix (-16) .&. 255
        green = shift pix (-8)  .&. 255
        blue  =       pix       .&. 255
        mono x = shift 255 24 .|. shift x 16 .|. shift x 8 .|. x
        trans = 0

    writeArray pixels i $
      if alpha == 255 && red == blue && green == 0
         then mono red
         else trans

  return (surface, w, h)



getSurface :: Engine -> FilePath -> Mask -> IO (Cairo.Surface, Double, Double)
getSurface (Engine { cache }) src mask = do
  cached <- Cairo.liftIO (readIORef cache)

  case (M.lookup (src, mask) cached, mask) of
    (Just (surface, w, h), _) -> do
      return (surface, w, h)

    (Nothing, NoMask) -> do
      surface <- Cairo.imageSurfaceCreateFromPNG $ normalise src
      (w, h) <- getSurfaceSize surface

      writeIORef cache (M.insert (src, mask) (surface, w, h) cached)
      return (surface, w, h)

    (Nothing, Mask) -> do
      (surface, _, _) <- createMask $ normalise src
      (w, h) <- getSurfaceSize surface
      writeIORef cache (M.insert (src, mask) (surface, w, h) cached)
      return (surface, w, h)

getSurfaceSize :: Cairo.Surface -> IO (Double, Double)
getSurfaceSize surface = do
  w <- Cairo.imageSurfaceGetWidth surface
  h <- Cairo.imageSurfaceGetHeight surface
  pure (fromIntegral w, fromIntegral h)

{-| A utility function for rendering a specific element. -}
renderElement :: Engine -> Element -> Cairo.Render ()
renderElement state (CollageElement w h center forms) = do
  Cairo.save
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.clip
  forM_ center $ uncurry Cairo.translate
  mapM_ (renderForm state) forms
  Cairo.restore

renderElement state (ImageElement crop Nothing src) = do
  (surface, w, h) <- Cairo.liftIO $ getSurface state src NoMask
  let (sx, sy, sw, sh) =
        case crop of
          Just (Crop (sx')
                     (sy')
                     (sw')
                     (sh')) -> (sx', sy', sw', sh')
          Nothing -> (0, 0, w, h)

  Cairo.save
  Cairo.translate (-sx) (-sy)
  Cairo.scale 1 1

  Cairo.setSourceSurface surface 0 0
  Cairo.translate sx sy
  Cairo.rectangle 0 0 sw sh
  Cairo.fill
  Cairo.restore

renderElement state (ImageElement crop (Just color) src) = do
  (surface, w, h) <- Cairo.liftIO $ getSurface state src NoMask
  (mask, _, _) <- Cairo.liftIO $ getSurface state src Mask
  let (Crop sx sy sw sh) = maybe (Crop 0 0 w h) id crop

  Cairo.save
  Cairo.translate (-sx) (-sy)
  Cairo.scale 1 1

  Cairo.setSourceSurface surface 0 0
  Cairo.translate sx sy
  Cairo.rectangle 0 0 sw sh
  Cairo.fillPreserve

  Cairo.setSourceSurface mask 0 0
  Cairo.fill

  unpackColFor color Cairo.setSourceRGBA
  Cairo.setOperator Cairo.OperatorHslColor
  Cairo.maskSurface mask 0 0
  Cairo.fill

  Cairo.restore

renderElement _ (TextElement (Text { textColor = (Color r g b a), .. })) = do
    Cairo.save

    layout <- Pango.createLayout textUTF8

    Cairo.liftIO $ Pango.layoutSetAttributes layout
      [ Pango.AttrFamily { paStart = i, paEnd = j, paFamily = textTypeface }
      , Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight }
      , Pango.AttrStyle  { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle }
      , Pango.AttrSize   { paStart = i, paEnd = j, paSize = textHeight }
      ]

    Pango.PangoRectangle x y w h <- fmap snd $ Cairo.liftIO $ Pango.layoutGetExtents layout

    let centering =
          case textAlignment of
            LeftAligned -> 0
            CenterAligned -> -w / 2
        vcentering =
          case textVAlignment of
            TopVAligned -> 0
            CenterVAligned -> -h / 2

    Cairo.translate (centering -x) (vcentering - y)

    for_ textStroke $ \ls -> do
      Pango.layoutPath layout
      setLineStyle ls

    Cairo.setSourceRGBA r g b a
    Pango.showLayout layout
    Cairo.restore

  where
    i = 0
    j = T.length textUTF8

{-| A utility function that maps to a Pango font weight based off our variant. -}
mapFontWeight :: FontWeight -> Pango.Weight
mapFontWeight weight = case weight of
  LightWeight  -> Pango.WeightLight
  NormalWeight -> Pango.WeightNormal
  BoldWeight   -> Pango.WeightBold

{-| A utility function that maps to a Pango font style based off our variant. -}
mapFontStyle :: FontStyle -> Pango.FontStyle
mapFontStyle style = case style of
  NormalStyle  -> Pango.StyleNormal
  ObliqueStyle -> Pango.StyleOblique
  ItalicStyle  -> Pango.StyleItalic

{-| A utility function that goes into a state of transformation and then pops it when finished. -}
withTransform :: Double -> Double -> Double -> Double -> Double -> Cairo.Render () -> Cairo.Render ()
withTransform sx sy t x y f = do
  Cairo.scale sx sy
  Cairo.translate x y
  Cairo.rotate t
  f

{-| A utility function that sets the Cairo line cap based off of our version. -}
setLineCap :: LineCap -> Cairo.Render ()
setLineCap cap = case cap of
  FlatCap   -> Cairo.setLineCap Cairo.LineCapButt
  RoundCap  -> Cairo.setLineCap Cairo.LineCapRound
  PaddedCap -> Cairo.setLineCap Cairo.LineCapSquare

{-| A utility function that sets the Cairo line style based off of our version. -}
setLineJoin :: LineJoin -> Cairo.Render ()
setLineJoin join = case join of
  SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
  SharpJoin lim -> Cairo.setLineJoin Cairo.LineJoinMiter >> Cairo.setMiterLimit lim
  ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a line style and then strokes afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle (LineStyle { lineColor = Color r g b a, .. }) = do
  Cairo.setSourceRGBA r g b a
  setLineCap lineCap
  setLineJoin lineJoin
  Cairo.setLineWidth lineWidth
  Cairo.setDash lineDashing lineDashOffset
  Cairo.stroke

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a fill style and then fills afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setFillStyle :: Engine -> FillStyle -> Cairo.Render ()
setFillStyle _ (Texture _) = error "you can't set a fill on a texture, dingus"
setFillStyle _ (Solid (Color r g b a)) = do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

-- setFillStyle state (Texture src) = do
--   (surface, _, _) <- Cairo.liftIO $ getSurface state src
--   Cairo.setSourceSurface surface 0 0
--   Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
--   Cairo.fill

{-| A utility that renders a form. -}
renderForm :: Engine -> Form -> Cairo.Render ()
renderForm state Form { .. } = withTransform formScaleX formScaleY formTheta formX formY $
  case formStyle of
    PathForm _ [] -> pure ()

    PathForm style ((hx, hy) : ps) -> do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (uncurry Cairo.lineTo) ps
      setLineStyle style

    ShapeForm style shape -> do
      case shape of
        PolygonShape ~ps@((hx, hy) : _) -> do
          Cairo.newPath
          Cairo.moveTo hx hy
          mapM_ (uncurry Cairo.lineTo) ps
          Cairo.closePath

        RectangleShape (w, h) -> do
          Cairo.newPath
          Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) -> do
          Cairo.newPath
          Cairo.scale sx sy
          Cairo.arc cx cy r a1 a2
          Cairo.scale 1 1

      either setLineStyle (setFillStyle state) style

    ElementForm element -> renderElement state element
    GroupForm mayhaps forms -> do
      Cairo.save
      forM_ mayhaps Cairo.setMatrix
      mapM_ (renderForm state) forms
      Cairo.restore

    CompositeForm (CompositeStyle alpha) form -> do
      Cairo.pushGroup
      renderForm state form
      Cairo.popGroupToSource
      Cairo.paintWithAlpha alpha



unpackColFor :: Color
             -> (Double -> Double ->  Double -> Double -> a)
             -> a
unpackColFor (Color r g b a) f = f r g b a
