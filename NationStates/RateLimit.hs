module NationStates.RateLimit (
    RateLimit(),
    newRateLimit,
    rateLimit,
    ) where


import Control.Concurrent
import System.Clock


data RateLimit = RateLimit {
    rateLock :: !(MVar TimeSpec),
    rateDelay :: !TimeSpec
    }


newRateLimit :: TimeSpec -> IO RateLimit
newRateLimit delay = do
    lock <- newMVar $! negate delay
    return RateLimit {
        rateLock = lock,
        rateDelay = delay
        }


rateLimit :: RateLimit -> IO a -> IO a
rateLimit RateLimit { rateLock = lock, rateDelay = delay } action =
    modifyMVar lock $ \prev -> do
        now <- getTime Monotonic
        threadDelay' $ prev + delay - now
        now' <- getTime Monotonic
        result <- action
        return (now', result)


threadDelay' :: TimeSpec -> IO ()
threadDelay' t = threadDelay . fromInteger $ timeSpecAsNanoSecs t `div` 1000
