{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( module Control.FRPNow.Core
    , module Control.FRPNow.Lib
    , whenE
    , foldp
    , poll
    , pollFold
    ) where

import Control.FRPNow.Core
import Control.FRPNow.Lib hiding (when)
import qualified Control.FRPNow.Lib as Lib (when)

whenE :: Behavior Bool -> Behavior (Event ())
whenE = Lib.when

foldp :: Eq a => (a -> b -> b) -> b -> Behavior a -> Behavior (Behavior b)
foldp f b a = foldB (flip f) b a

poll :: Now a -> Now (Behavior a)
poll io = loop
  where
    loop = do
        e  <- async (return ())
        a <- io
        e' <- planNow $ loop <$ e
        return $ pure a `switch` e'

pollFold :: Now a -> (a -> Now a) -> Now (Behavior a)
pollFold init io = do
    first <- init
    loop first
  where
    loop prev = do
        e  <- async (return ())
        a <- io prev
        e' <- planNow $ loop a <$ e
        return $ pure a `switch` e'

