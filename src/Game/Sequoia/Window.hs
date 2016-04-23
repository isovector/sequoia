module Game.Sequoia.Window
    ( getDimensions
    ) where

import Control.FRPNow.Core
import Control.Monad.IO.Class (liftIO)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import GHC.Ptr (Ptr)
import Game.Sequoia.Engine
import Game.Sequoia.Signal
import qualified Graphics.UI.SDL as SDL

getDimensions :: Engine -> N (B (Int, Int))
getDimensions = liftWindow SDL.getWindowSize

position :: Engine -> N (B (Int, Int))
position = liftWindow SDL.getWindowPosition

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

