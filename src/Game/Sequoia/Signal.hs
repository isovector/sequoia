module Game.Sequoia.Signal
    ( Signal(..)
    , Address()
    -- * Composing
    , signal
    , effectful
    , mailbox
    , mailboxs
    , mail
    , mail'
    , delay
    , constant
    , sampleAt
    , (<$>)
    , (<*>)
    -- * Accumulating
    , foldp
    , countIf
    ) where

import Control.Monad (ap, liftM2, when, forM_)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class
import Data.IORef (IORef (..), newIORef, readIORef, writeIORef, modifyIORef)
import Data.Traversable (sequenceA)
import System.IO.Unsafe (unsafePerformIO)

newtype Signal a = Signal { runSignal :: Int -> IO a }
newtype Address a = Address { runMailbox :: Int -> a -> IO () }

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

sampleAt :: Int -> Signal a -> IO a
sampleAt = flip runSignal

mailboxs :: (a -> a -> a) -> a -> IO (Signal a, Address a)
mailboxs f a = do
    ref <- newIORef [(0, a)]
    return ( Signal $ \i -> do
                times <- readIORef ref
                return . snd . head $ dropWhile ((> i) . fst) times
           , Address $ \i a -> modifyIORef ref $
               \contents -> (i, f (snd $ head contents) a) : contents
           )

mailbox :: a -> IO (Signal a, Address a)
mailbox = mailboxs (flip const)

mail :: Address a -> a -> Signal ()
mail addr a = Signal $ \i -> runMailbox addr (i + 1) a

mail' :: Address a -> a -> IO ()
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

{-# NOINLINE foldp #-}
foldp :: (a -> b-> b) -> b -> Signal a -> Signal b
foldp f s sa = unsafePerformIO $ do
    latest <- newIORef (0, s)
    return . Signal $ \i -> do
        (time, val) <- readIORef latest
        case compare i time of
            GT -> update i (Just latest) time val
            LT -> update i Nothing 0 s
            EQ -> return val
  where
    update i mlatest time val | i == time = return val
                              | otherwise = do
        let latestf = maybe (const $ return ())
                            writeIORef
                            mlatest
        a <- runSignal sa i
        let newval = f a val
        latestf (i, newval)
        update i mlatest (time + 1) newval

countIf :: (a -> Bool) -> Signal a -> Signal Int
countIf f = foldp (\v c -> c + fromEnum (f v)) 0

