{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( module Control.FRPNow.Core
    , module Control.FRPNow.Lib
    , whenE
    , foldp
    , foldmp
    , poll
    , pollFold
    , mailbox
    -- TODO(sandy): remove these after the migration
    , Address
    ) where

import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.Lib hiding (when)
import qualified Control.FRPNow.Lib as Lib (when)

type Address a = a -> IO ()

whenE :: Behavior Bool -> Behavior (Event ())
whenE = Lib.when

foldp :: Eq a => (a -> b -> b) -> b -> Behavior a -> Behavior (Behavior b)
foldp f b a = foldB (flip f) b a

foldmp :: Eq a
       => a
       -> (a -> Now a)
       -> Now (Behavior a, Address (a -> a))
foldmp da f = do
    (sa, mb) <- callbackStream
    let es = next sa
    se <- sample es
    b <- loop da es se
    return (b, mb)
  where
    loop a es se = do
        e  <- async (return ())
        seMay <- sample $ tryGetEv se
        (a', se') <-
            case seMay of
              Just g  -> sample es >>= return . (g a,)
              Nothing -> return (a, se)
        a'' <- f a'
        e' <- planNow $ loop a'' es se' <$ e
        return $ pure a'' `switch` e'

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

mailbox :: a -> Now (Behavior a, Address a)
mailbox a = do
    (mailbox, send) <- callbackStream
    signal <- sample $ fromChanges a mailbox
    return (signal, send)

