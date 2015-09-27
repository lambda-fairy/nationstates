module NationStates (

    Context(),
    newContext,
    newContext',

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
newContext userAgent = newContext' userAgent True


-- | Create a new 'Context', with extra options.
newContext'
    :: String
        -- ^ User agent
    -> Bool
        -- ^ Use HTTPS (experimental)
    -> IO Context
newContext' userAgent isSecure = do
    man <- newManager $
        if isSecure
            then tlsManagerSettings
            else defaultManagerSettings
    limit <- newRateLimit 0.6
    return Context {
        contextManager = man,
        contextRateLimit = rateLimit limit,
        contextIsSecure = isSecure,
        contextUserAgent = userAgent
        }
