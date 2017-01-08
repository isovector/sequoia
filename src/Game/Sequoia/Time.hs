module Game.Sequoia.Time
    ( Time
    , Clock ()
    , getClock
    , totalTime
    , deltaTime
    ) where

import Control.Applicative ((<$>))
import Control.FRPNow.Core
import Control.FRPNow.Lib hiding (first)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Game.Sequoia.Signal
import Prelude hiding (last)

type Time = Double
data Clock = Clock (B Time) (B Time)

getClock :: N Clock
getClock = do
    now <- realToFrac <$> sync getPOSIXTime
    clock <- poll . sync $ getTime now
    elapsed <- sample $ do
        first <- sample clock
        last  <- prev first clock
        return $ (-) <$> clock <*> last
    return $ Clock clock elapsed

totalTime :: Clock -> B Time
totalTime (Clock a _) = a

deltaTime :: Clock -> B Time
deltaTime (Clock _ a) = a

getTime :: Time -> IO Time
getTime t = subtract t . realToFrac <$> getPOSIXTime

