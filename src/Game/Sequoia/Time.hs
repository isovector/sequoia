module Game.Sequoia.Time
    ( Time
    , getClock
    , getElapsedClock
    , getFpsScheduler
    ) where

import Control.Applicative ((<$>))
import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.Lib
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Game.Sequoia.Signal
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak

type Time = Double

-- elapsed :: Signal Time
-- elapsed = fmap (uncurry (-)) . unsafePerformIO $ do
--     start <- getTime
--     return . foldp update (start, start) $ liftIO getTime
--   where
--     update new (old, _) = (new, old)

getClock :: Behavior (Event ()) -> Now (Behavior Time)
getClock = scheduled getTime

getElapsedClock :: Behavior (Event ()) -> Now (Behavior Time)
getElapsedClock schedule = do
    clock <- getClock schedule
    sample $ elapsed clock

elapsed :: Behavior Time -> Behavior (Behavior Time)
elapsed clock = do
    first <- sample clock
    last  <- prev first clock
    return $ (-) <$> clock <*> last

getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime

change' :: (a -> a -> Bool) -> (a -> b) -> Behavior a -> Behavior (Event b)
change' p f b = fmap (fmap f) . futuristic $ do
    v <- b
    whenJust (notSame v <$> b)
  where
    notSame v v' | not $ p v v' = Just v'
                 | otherwise    = Nothing

getFpsScheduler :: Double -> Now (Behavior (Event ()))
getFpsScheduler frames = do
    schedule <- getScheduler
    fmap (change' (on (==) snd) (const ())) $ scheduledFold
        schedule
        (getTime >>= \a -> return (a, a))
        $ \(_, last) -> do
            now' <- getTime
            return (now',
                   if now' - last > dt
                      then now'
                      else last
                      )
  where
    dt = 1 / frames

