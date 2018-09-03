{-# OPTIONS_GHC -funbox-strict-fields #-}

module Game.Sequoia.Engine
    ( Engine (..)
    , Mask (..)
    ) where

import           Data.IORef (IORef)
import           Data.Map (Map)
import           Game.Sequoia.Color
import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL.Raw as SDL

data Mask = Mask | NoMask deriving (Eq, Show, Ord, Bounded, Enum)

data Engine = Engine
    { window      :: !SDL.Window
    , renderer    :: !SDL.Renderer
    , continue    :: !Bool
    , backColor   :: !Color
    , cache       :: !(IORef (Map (FilePath, Mask) (Cairo.Surface, Double, Double)))
    , textureSize :: !(IORef (Int, Int))
    , buffer      :: !(IORef (SDL.Texture))
    }

