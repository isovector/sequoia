module Game.Sequoia.Time
    ( Time
    , elapsed
    , time
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Game.Sequoia.Signal
import System.IO.Unsafe (unsafePerformIO)

type Time = Double

elapsed :: Signal Time
elapsed = fmap (uncurry (-)) . unsafePerformIO $ do
    start <- getTime
    return . foldp update (start, start) $ liftIO getTime
  where
    update new (old, _) = (new, old)

time :: Signal Time
time = liftIO getTime

getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime

