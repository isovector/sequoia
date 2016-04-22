{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( Signal -- TODO(sandy): remove this later
    , module Control.FRPNow.Core
    , module Control.FRPNow.Lib
    , whenE
    , foldp
    , scheduled
    , scheduledFold
    , getScheduler
    ) where

import Control.FRPNow.Core
import Control.FRPNow.Lib hiding (when)
import qualified Control.FRPNow.Lib as Lib (when)

type Signal = Behavior

whenE :: Behavior Bool -> Behavior (Event ())
whenE = Lib.when

foldp :: Eq a => (a -> b -> b) -> b -> Behavior a -> Behavior (Behavior b)
foldp f b a = foldB (flip f) b a

getScheduler :: Now (Behavior (Event ()))
getScheduler = loop
  where
    loop = do
        (e, io) <- callback
        async $ io ()
        e' <- planNow $ loop <$ e
        return $ pure e `switch` e'

scheduled :: Now a -> Behavior (Event ()) -> Now (Behavior a)
scheduled io es = scheduledFold es (return undefined) (const io)

scheduledFold :: Behavior (Event ())
              -> Now a
              -> (a -> Now a)
              -> Now (Behavior a)
scheduledFold es iofirst io = do
    first <- iofirst
    loop first
  where
    loop prev = do
        e  <- sample es
        a <- io prev
        e' <- planNow $ loop a <$ e
        return $ pure a `switch` e'

