module NationStates (

    -- * Tracking connections
    withManagerNS,
    Manager,

    -- * Core types
    module NationStates.Types,

    ) where


import Network.HTTP.Client
import Network.HTTP.Client.TLS

import NationStates.Types


-- | Construct a connection 'Manager', with settings tailored to the
-- NationStates API.
--
-- @
-- withManagerNS = 'withManager' 'tlsManagerSettings'
-- @
withManagerNS :: (Manager -> IO a) -> IO a
withManagerNS = withManager tlsManagerSettings
