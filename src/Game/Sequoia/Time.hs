module Game.Sequoia.Time
    ( Time
    , getClock
    , getElapsedClock
    ) where

import Control.Applicative ((<$>))
import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.Lib
import Control.Monad.IO.Class (liftIO)
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

