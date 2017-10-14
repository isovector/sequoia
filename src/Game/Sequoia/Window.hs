module Game.Sequoia.Window
    ( getDimensions
    , mousePos
    , mouseButtons
    , MouseButton (..)
    ) where

import           Control.FRPNow.Core
import           Foreign.C.Types (CInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable
import           GHC.Ptr (Ptr)
import           Game.Sequoia.Engine
import           Game.Sequoia.Signal
import           SDL.Input.Mouse (MouseButton (..), getMouseButtons)
import qualified SDL.Raw as SDL

getDimensions :: Engine -> N (B (Int, Int))
getDimensions = liftWindow SDL.getWindowSize

-- position :: Engine -> N (B (Int, Int))
-- position = liftWindow SDL.getWindowPosition


mousePos :: N (B (Int, Int))
mousePos = poll $ sync lifted
  where
    lifted =
      alloca $ \xptr ->
      alloca $ \yptr -> do
        _ <- SDL.getMouseState xptr yptr
        x <- peek xptr
        y <- peek yptr
        pure (fromIntegral x, fromIntegral y)


mouseButtons :: N (B (MouseButton -> Bool))
mouseButtons = poll $ sync getMouseButtons


liftWindow :: (SDL.Window -> Ptr CInt -> Ptr CInt -> IO ())
           -> Engine
           -> N (B (Int, Int))
liftWindow f e = poll $ sync lifted
  where
    lifted =
        alloca $ \wptr ->
        alloca $ \hptr -> do
            f (window e) wptr hptr
            w <- peek wptr
            h <- peek hptr
            return (fromIntegral w, fromIntegral h)

