{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( Signal
    , foldp
    ) where

import Control.Monad
import Control.FRPNow.Core
import Control.FRPNow.Lib

type Signal = Behavior

foldp :: Eq a => (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b a = join $ foldB (flip f) b a
