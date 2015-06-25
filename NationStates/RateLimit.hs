-- | Simple rate limiting combinator.

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


-- | Create a new rate limiter with the specified delay.
--
-- The rate limiter is thread-safe, and can be shared between threads.
newRateLimit :: TimeSpec -> IO RateLimit
newRateLimit delay = do
    lock <- newMVar $! negate delay
    return RateLimit {
        rateLock = lock,
        rateDelay = delay
        }


-- | Run the given action, pausing as necessary to keep under the rate limit.
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
