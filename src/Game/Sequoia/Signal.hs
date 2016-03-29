{-# LANGUAGE TupleSections #-}
module Game.Sequoia.Signal
    ( Signal(..)
    , Edge (..)
    , Address()
    , value
    , changed
    , signal
    , effectful
    , mailbox
    , newMailbox
    , mail
    , mail'
    , delay
    , constant
    , sampleAt
    , (<$>)
    , (<*>)
    , countIf
    , foldp
    , foldmp
    , edges
    , edges'
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (ap, liftM2, when, forM_)
import Control.Monad.IO.Class
import Data.IORef
import Data.Traversable (sequenceA)
import System.IO.Unsafe (unsafePerformIO)

newtype Signal a = Signal { runSignal :: Int -> IO a }
newtype Address a = Address { runMailbox :: Int -> (a -> a) -> IO () }

instance Functor Signal where
    fmap f = Signal . fmap (fmap f) . runSignal

instance Applicative Signal where
    pure = Signal . const . pure
    Signal f <*> Signal a = Signal $ liftM2 ap f a

instance Monad Signal where
    return = pure
    Signal sa >>= f = Signal $ \i -> do
        a <- sa i
        runSignal (f a) i

instance MonadIO Signal where
    liftIO = Signal . const

data Edge a = Changed a
            | Unchanged a
            deriving (Eq, Show)

value :: Edge a -> a
value (Changed a)   = a
value (Unchanged a) = a

changed :: Edge a -> Bool
changed (Changed _) = True
changed _           = False

sampleAt :: Int -> Signal a -> IO a
sampleAt = flip runSignal

mailbox :: a -> IO (Signal a, Address a)
mailbox a = do
    ref <- newIORef [(0, a)]
    return ( Signal $ \i -> do
                times <- readIORef ref
                return . snd . head $ dropWhile ((> i) . fst) times
           , Address $ \i f -> modifyIORef ref $
               \contents -> (i, f . snd $ head contents) : contents
           )

{-# NOINLINE newMailbox #-}
newMailbox :: String -> a -> (Signal a, Address a)
newMailbox _ a = unsafePerformIO $ mailbox a

mail :: Address a -> (a -> a) -> Signal ()
mail addr f = Signal $ \i -> runMailbox addr (i + 1) f

mail' :: Address a -> (a -> a) -> IO ()
mail' = flip runMailbox 0

signal :: (Int -> a) -> Signal a
signal f = Signal $ return . f

effectful :: (Int -> IO a) -> Signal a
effectful = Signal

delay :: a -> Int -> Signal a -> Signal a
delay a d (Signal f) = Signal $ \i -> do
    if i == 0
       then pure a
       else f $ i - d

constant :: a -> Signal a
constant = pure

countIf :: Eq a => (a -> Bool) -> Signal a -> Signal Int
countIf f = foldp ((+) . fromEnum . f) 0

{-# NOINLINE foldp #-}
foldp :: Eq b => (a -> b-> b) -> b -> Signal a -> Signal b
foldp f s sa = fst . foldmp s $ \b -> sa >>= (return . flip f b)

{-# NOINLINE foldmp #-}
-- TODO(sandy): this is less well tested than I might want
foldmp :: Eq a => a -> (a -> Signal a) -> (Signal a, Address a)
foldmp da f = unsafePerformIO $ do
    (sa, mb) <- first edges <$> mailbox da
    latest <- newIORef (0, da)
    return
        . (, mb)
        . Signal $ \i -> do
            (time, val) <- readIORef latest
            case compare i time of
                GT -> update sa i (Just latest) time val
                LT -> update sa i Nothing 0 da
                EQ -> return val
  where
    update sa i mlatest time val' | i == time = return val'
                                  | otherwise = do
        let latestf = maybe (const $ return ())
                            writeIORef
                            mlatest
        c <- runSignal sa i
        let val = case c of
                    Changed   a -> a
                    Unchanged _ -> val'

        newval <- runSignal (f val) i
        latestf (i, newval)
        update sa i mlatest (time + 1) newval


edges' :: (a -> a -> Bool) -> Signal a -> Signal (Edge a)
edges' f sa = Signal $ \i -> do
    new <- runSignal sa i
    if i == 0
       then return $ Changed new
       else do
           -- TODO(sandy): egregiously bad behavior over foldp
           old <- runSignal sa (i - 1)
           return $ (if f old new then Unchanged else Changed) new

edges :: Eq a => Signal a -> Signal (Edge a)
edges = edges' (==)

