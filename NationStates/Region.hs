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
    gavote,
    scvote,
    founder,
    power,
    flag,
    embassies,
    tags,
--    happenings,
--    messages,
--    history,
--    poll,

    ) where


import Control.Applicative
import Control.Monad
import Text.XML.Light
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
numnations = Region . fmap (expect "nation count" <*> readMaybe) $
    makeNS "numnations" "NUMNATIONS"

-- | List of nations in the region.
--
-- > ["urmanian","enatai","unfitting_doors","lykosia","trotterdam"]
nations :: Region [String]
nations = Region . fmap (wordsBy (== ':')) $ makeNS "nations" "NATIONS"

-- | Region delegate.
--
-- Returns @Nothing@ when the region has no delegate.
--
-- > Just "princess_luna"
delegate :: Region (Maybe String)
delegate = Region . fmap (pureIf (/= "0")) $ makeNS "delegate" "DELEGATE"

-- | The number of endorsements earned by the delegate.
--
-- Returns @0@ when the region has no delegate.
--
-- > 22
delegatevotes :: Region Integer
delegatevotes = Region . fmap (expect "delegate vote count" <*> readMaybe) $
    makeNS "delegatevotes" "DELEGATEVOTES"

-- | The number of votes for and against the current General Assembly
-- resolution.
--
-- Returns @Nothing@ when there is no proposal at vote.
--
-- > Just (28,11)
gavote :: Region (Maybe (Integer, Integer))
gavote = Region $ makeNS' "gavote" Nothing [] parse
  where
    parse _ = expect "GA vote counts" <$> showElement <*>
        (grabVotes <=< findChild (unqual "GAVOTE"))

-- | The number of votes for and against the current Security Council
-- resolution.
--
-- Returns @Nothing@ when there is no proposal at vote.
--
-- > Just (20,34)
scvote :: Region (Maybe (Integer, Integer))
scvote = Region $ makeNS' "scvote" Nothing [] parse
  where
    parse _ = expect "SC vote counts" <$> showElement <*>
        (grabVotes <=< findChild (unqual "SCVOTE"))

grabVotes :: Element -> Maybe (Maybe (Integer, Integer))
grabVotes root = do
    forStr <- grab "FOR"
    againstStr <- grab "AGAINST"
    return $ (,) <$> readMaybe forStr <*> readMaybe againstStr
  where
    grab childName = strContent <$> findChild (unqual childName) root

-- | Region founder.
--
-- Returns @Nothing@ when the region is founderless.
--
-- > Just "magical_equestria"
founder :: Region (Maybe String)
founder = Region . fmap (pureIf (/= "0")) $ makeNS "founder" "FOUNDER"

-- | Regional power.
--
-- > "High"
power :: Region String
power = Region $ makeNS "power" "POWER"

-- | Regional flag.
--
-- > Just "http://www.nationstates.net/images/flags/uploads/rflags/pony_lands__478033.png"
flag :: Region (Maybe String)
flag = Region . fmap (pureIf (/= "")) $ makeNS "flag" "FLAG"

-- | Region embassies.
--
-- > ["New Lunar Republic","Tareldanore"]
embassies :: Region [String]
embassies = Region $ makeNS' "embassies" Nothing [] parse
  where
    parse _ = expect "embassy names" <$> showElement <*>
        fmap (grabChildren "EMBASSY") . findChild (unqual "EMBASSIES")

-- | Region tags.
--
-- > ["Silly","Monarchist","Large"]
tags :: Region [String]
tags = Region $ makeNS' "tags" Nothing [] parse
  where
    parse _ = expect "region tags" <$> showElement <*>
        fmap (grabChildren "TAG") . findChild (unqual "TAGS")

grabChildren :: String -> Element -> [String]
grabChildren childName = map strContent . findChildren (unqual childName)
