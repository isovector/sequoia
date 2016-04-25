{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( module Control.FRPNow.Core
    , module Control.FRPNow.Lib
    , whenE
    , soonest
    , foldp
    , foldmp
    , poll
    , pollFold
    , mailbox
    -- TODO(sandy): remove these after the migration
    , Address
    , B
    , N
    , E
    ) where

import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.Lib hiding (when, first)
import qualified Control.FRPNow.Lib as Lib (when, first)

type Address a = a -> IO ()
type B = Behavior
type E = Event
type N = Now

soonest :: E a -> E a -> B (E a)
soonest = Lib.first

whenE :: B Bool -> B (E ())
whenE = Lib.when

foldp :: Eq a => (a -> b -> b) -> b -> B a -> B (B b)
foldp f b a = foldB (flip f) b a

foldmp :: a
       -> (a -> N a)
       -> N (B a, Address (a -> a))
foldmp da f = do
    (sa, mb) <- callbackStream
    let es = nextAll sa
    se <- sample es
    b  <- loop da es se
    return (b, mb)
  where
    loop a es se = do
        e     <- async (return ())
        seMay <- sample $ tryGetEv se
        (a', se') <-
            case seMay of
              Just gs  -> sample es >>= return . (foldr ($) a gs,)
              Nothing -> return (a, se)
        a'' <- f a'
        e'  <- planNow $ loop a'' es se' <$ e
        return $ step a'' e'

poll :: N a -> N (B a)
poll io = loop
  where
    loop = do
        e  <- async (return ())
        a  <- io
        e' <- planNow $ loop <$ e
        return $ step a e'

pollFold :: N a -> (a -> N a) -> N (B a)
pollFold init io = do
    first <- init
    loop first
  where
    loop prev = do
        e  <- async (return ())
        a  <- io prev
        e' <- planNow $ loop a <$ e
        return $ step a e'

mailbox :: a -> N (B a, Address a)
mailbox a = do
    (mailbox, send) <- callbackStream
    signal <- sample $ fromChanges a mailbox
    return (signal, send)

