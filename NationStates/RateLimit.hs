-- | Simple rate limiting combinator.

module NationStates.RateLimit (
    RateLimit(),
    newRateLimit,
    rateLimit,
    setDelay,
    ) where


import Control.Concurrent
import Control.Exception
import System.Clock


data RateLimit = RateLimit {
    _rateLock :: !(MVar TimeSpec),
    _rateDelay :: !TimeSpec
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
    return $ RateLimit lock delay
  where
    delay = fromSeconds delay'


-- | Run the given action, pausing as necessary to keep under the rate limit.
rateLimit :: RateLimit -> IO a -> IO a
rateLimit (RateLimit lock delay) action =
    mask $ \restore -> do
        prev <- takeMVar lock
        now <- getTime Monotonic
        threadDelay' (prev + delay - now) `onException` putMVar lock prev
        restore action `finally` (putMVar lock =<< getTime Monotonic)


threadDelay' :: TimeSpec -> IO ()
threadDelay' t = threadDelay . fromInteger $ timeSpecAsNanoSecs t `div` 1000


-- | Create a new rate limiter with the same lock but a different delay.
setDelay :: Rational -> RateLimit -> RateLimit
setDelay delay' (RateLimit lock _) = RateLimit lock (fromSeconds delay')


fromSeconds :: Rational -> TimeSpec
fromSeconds n = fromInteger . ceiling $ n * 1000 * 1000 * 1000
