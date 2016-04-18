{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( Signal -- TODO(sandy): remove this later
    , module Control.FRPNow.Core
    , module Control.FRPNow.Lib
    , whenE
    , foldp
    ) where

import Control.FRPNow.Core
import Control.FRPNow.Lib hiding (when)
import qualified Control.FRPNow.Lib as Lib (when)

type Signal = Behavior

whenE :: Behavior Bool -> Behavior (Event ())
whenE = Lib.when

foldp :: Eq a => (a -> b -> b) -> b -> Behavior a -> Behavior (Behavior b)
foldp f b a = foldB (flip f) b a
