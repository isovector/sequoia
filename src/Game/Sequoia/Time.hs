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

getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime

getClock :: Double -> Now (Behavior Double)
getClock precision =
  do start <- sync getTime
     (res,cb) <- callbackStream
     wres<- sync $ mkWeakPtr res Nothing
     let getDiff = (subtract start) <$> getTime
     let onTimeOut =
              deRefWeak wres >>= \x ->
                 case x of
                   Just _ -> getDiff >>= cb >> return True
                   Nothing -> return False
     sample $ fromChanges 0 res
