{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Region API.
--
-- This module should be imported qualified, to prevent name clashes:
--
-- @
-- import NationStates
-- import qualified NationStates.Region as Region
-- @
--
-- In general, this module follows the terminology used in the
-- <https://www.nationstates.net/pages/api.html#regionapi official documentation>.
--
-- Here's a short example:
--
-- @
-- import NationStates
-- import qualified NationStates.Region as Region
-- import Text.Printf
--
-- main = do
--     c <- 'NationStates.newContext' "ExampleBot/2000"
--     (name, numnations, delegate) <- Region.'run' "Pony Lands"
--         ((,,) \<$\> Region.'name' \<*\> Region.'numnations' \<*\> Region.'delegate') c
--     printf "%s has %d nations. Its delegate is %s\\n" name numnations delegate
-- @

module NationStates.Region (

    -- * Running queries
    Region(..),
    run,

    -- * Shards
    name,
    factbook,
    numnations,
    nations,
    delegate,
    delegatevotes,
--    gavote,
--    scvote,
    founder,
    power,
    flag,
--    embassies,
--    tags,
--    happenings,
--    messages,
--    history,
--    poll,

    ) where


import Control.Applicative
import Prelude  -- GHC 7.10

import NationStates.Core


-- | A request to the Region API.
newtype Region a = Region { unRegion :: NS a }
    deriving (Functor, Applicative)


-- | Perform a request to the Region API.
run
    :: String
        -- ^ Region name
    -> Region a
        -- ^ Requested shards
    -> Context
        -- ^ Connection manager
    -> IO a
run region = requestNS (Just ("region", region)) . unRegion


-- | Region name.
--
-- > "Pony Lands"
name :: Region String
name = Region $ makeNS "name" "NAME"

-- | Factbook, in BBCode format.
--
-- > "[b]We&#39;ve got ponies, therefore your argument is invalid..."
factbook :: Region String
factbook = Region $ makeNS "factbook" "FACTBOOK"

-- | Number of nations in the region.
--
-- > 112
numnations :: Region Integer
numnations = Region . fmap (expect "nation count" readMaybe) $
    makeNS "numnations" "NUMNATIONS"

-- | List of nations in the region.
--
-- > ["urmanian","enatai","unfitting_doors","lykosia","trotterdam"]
nations :: Region [String]
nations = Region . fmap (wordsBy (== ':')) $ makeNS "nations" "NATIONS"

-- | Region delegate.
--
-- > "princess_luna"
delegate :: Region String
delegate = Region $ makeNS "delegate" "DELEGATE"

-- | The number of endorsements earned by the delegate.
--
-- > 22
delegatevotes :: Region Integer
delegatevotes = Region . fmap (expect "delegate endorsement count" readMaybe) $
    makeNS "delegatevotes" "DELEGATEVOTES"

-- | Region founder.
--
-- > "magical_equestria"
founder :: Region String
founder = Region $ makeNS "founder" "FOUNDER"

-- | Regional power.
--
-- > "High"
power :: Region String
power = Region $ makeNS "power" "POWER"

-- | Regional flag.
--
-- > "http://www.nationstates.net/images/flags/uploads/rflags/pony_lands__478033.png"
flag :: Region String
flag = Region $ makeNS "flag" "FLAG"
