module Game.Sequoia.Time
    ( Time
    , getClock
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.Lib
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak

type Time = Double

-- elapsed :: Signal Time
-- elapsed = fmap (uncurry (-)) . unsafePerformIO $ do
--     start <- getTime
--     return . foldp update (start, start) $ liftIO getTime
--   where
--     update new (old, _) = (new, old)

getClock :: Now (Behavior Time)
getClock = do
    now <- sync getTime
    loop now
  where
    loop last = do
        now <- sync getTime
        e  <- async (return ())
        e' <- planNow $ loop now <$ e
        return $ pure (now - last) `switch` e'

getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime

