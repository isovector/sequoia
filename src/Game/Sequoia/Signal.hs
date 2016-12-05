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
    , onEvent
    , scanle
    , newCollection
    , B
    , N
    , E
    ) where

import Control.Monad (void)
import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.Lib hiding (when, first)
import Data.Map (Map)
import qualified Control.FRPNow.Lib as Lib (when, first)
import qualified Data.Map as M

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
       -> N (B a, (a -> a) -> IO ())
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
pollFold initial io = do
    first <- initial
    loop first
  where
    loop a = do
        e  <- async (return ())
        a'  <- io a
        e' <- planNow $ loop a' <$ e
        return $ step a' e'

mailbox :: a -> N (B a, a -> IO ())
mailbox a = do
    (mb, send) <- callbackStream
    signal <- sample $ fromChanges a mb
    return (signal, send)

onEvent :: B (E a) -> (a -> N ()) -> N ()
onEvent evs f = loop
  where
    loop :: Now ()
    loop = do
        e <-  sample evs
        void $ planNow $ (\a -> f a >> loop) <$> e

scanle :: (a -> b -> b)
       -> b
       -> N (B b, a -> IO ())
scanle f start = do
    (es, mb) <- callbackStream
    folded   <- sample $ scanlEv (flip f) start es
    b        <- sample $ fromChanges start folded
    return (b, mb)

newCollection :: Ord k
              => Map k v
              -> N ( k -> B (Maybe v)
                   , k -> v -> IO ()
                   )
newCollection start = do
    (b, mb) <- scanle (uncurry M.insert) start
    return ((<$> b) . M.lookup, curry mb)

