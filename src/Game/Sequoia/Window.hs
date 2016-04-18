module Game.Sequoia.Window
    ( dimensions
    , position
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

dimensions :: Engine -> Now (Behavior (Int, Int))
dimensions = liftWindow SDL.getWindowSize

position :: Engine -> Now (Behavior (Int, Int))
position = liftWindow SDL.getWindowPosition

liftWindow :: (SDL.Window -> Ptr CInt -> Ptr CInt -> IO ())
           -> Engine
           -> Now (Behavior (Int, Int))
liftWindow f e = lifted
  where
    lifted = liftIO .
        alloca $ \wptr ->
        alloca $ \hptr -> do
            f (window e) wptr hptr
            w <- peek wptr
            h <- peek hptr
            return $ return (fromIntegral w, fromIntegral h)

