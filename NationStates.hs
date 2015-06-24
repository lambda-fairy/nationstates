module NationStates (

    Context(),
    withContext,

    -- * Core types
    module NationStates.Types,

    ) where


import Network.HTTP.Client
import Network.HTTP.Client.TLS

import NationStates.Core
import NationStates.RateLimit
import NationStates.Types


withContext :: (Context -> IO a) -> IO a
withContext f = withManager tlsManagerSettings $ \man -> do
    limit <- newRateLimit delay
    f Context {
        contextManager = man,
        contextRateLimit = rateLimit limit
        }
  where
    delay = 600 * 1000 * 1000  -- 0.6 seconds
