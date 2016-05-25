module Game.Sequoia.Time
    ( Time
    , Clock ()
    , getClock
    , totalTime
    , deltaTime
    , fps
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
data Clock = Clock (B Time) (B Time)

getClock :: N Clock
getClock = do
    clock <- poll $ sync getTime
    elapsed <- sample $ do
        first <- sample clock
        last  <- prev first clock
        return $ (-) <$> clock <*> last
    return $ Clock clock elapsed

totalTime :: Clock -> B Time
totalTime (Clock a _) = a

deltaTime :: Clock -> B Time
deltaTime (Clock _ a) = a

getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime

change' :: (a -> a -> Bool) -> (a -> b) -> B a -> B (E b)
change' p f b = fmap (fmap f) . futuristic $ do
    v <- b
    whenJust (notSame v <$> b)
  where
    notSame v v' | not $ p v v' = Just v'
                 | otherwise    = Nothing

fps :: Double -> N (B (E ()))
fps frames = do
    fmap (change' (on (==) snd) (const ())) $ pollFold
        (sync getTime >>= \a -> return (a, a))
        $ \(_, last) -> do
            now' <- sync getTime
            return (now',
                   if now' - last > dt
                      then now'
                      else last
                      )
  where
    dt = 1 / frames

