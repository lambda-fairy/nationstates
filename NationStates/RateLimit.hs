-- | Simple rate limiting combinator.

module NationStates.RateLimit (
    RateLimit(),
    newRateLimit,
    rateLimit,
    ) where


import Control.Concurrent
import Control.Exception
import System.Clock


data RateLimit = RateLimit {
    rateLock :: !(MVar TimeSpec),
    rateDelay :: !TimeSpec
    }


-- | Create a new rate limiter with the specified delay.
--
-- The rate limiter is thread-safe, and can be shared between threads.
newRateLimit
    :: Rational
        -- ^ Delay, in seconds
    -> IO RateLimit
newRateLimit delay' = do
    lock <- newMVar $! negate delay
    return RateLimit {
        rateLock = lock,
        rateDelay = delay
        }
  where
    delay = fromInteger . ceiling $ delay' * 1000 * 1000 * 1000


-- | Run the given action, pausing as necessary to keep under the rate limit.
rateLimit :: RateLimit -> IO a -> IO a
rateLimit RateLimit { rateLock = lock, rateDelay = delay } action =
    bracketOnError
        (takeMVar lock)
        (tryPutMVar lock)
        (\prev -> do
            now <- getTime Monotonic
            threadDelay' $ prev + delay - now
            action `finally` (putMVar lock =<< getTime Monotonic))


threadDelay' :: TimeSpec -> IO ()
threadDelay' t = threadDelay . fromInteger $ timeSpecAsNanoSecs t `div` 1000
