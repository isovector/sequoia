module Game.Sequoia.Window
    ( dimensions
    , position
    ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import GHC.Ptr (Ptr)
import Game.Sequoia.Engine
import Game.Sequoia.Signal
import qualified Graphics.UI.SDL as SDL

dimensions :: Signal (Int, Int)
dimensions = liftWindow SDL.getWindowSize

position :: Signal (Int, Int)
position = liftWindow SDL.getWindowPosition

liftWindow :: (SDL.Window -> Ptr CInt -> Ptr CInt -> IO ())
           -> Signal (Int, Int)
liftWindow f = engine >>= lifted
  where
    lifted e = liftIO $
        alloca $ \wptr ->
        alloca $ \hptr -> do
            f (window e) wptr hptr
            w <- peek wptr
            h <- peek hptr
            return (fromIntegral w, fromIntegral h)

