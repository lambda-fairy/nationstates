module NationStates (

    Context(),
    newContext,

    -- * Core types
    module NationStates.Types,

    ) where


import Network.HTTP.Client
import Network.HTTP.Client.TLS

import NationStates.Core
import NationStates.RateLimit
import NationStates.Types


-- | Create a new 'Context'.
newContext
    :: String
        -- ^ User agent
    -> IO Context
newContext userAgent = do
    man <- newManager tlsManagerSettings
    limit <- newRateLimit delay
    return Context {
        contextManager = man,
        contextRateLimit = rateLimit limit,
        contextUserAgent = userAgent
        }
  where
    delay = fromInteger $ 600 * 1000 * 1000  -- 0.6 seconds
