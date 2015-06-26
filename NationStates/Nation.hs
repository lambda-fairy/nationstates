{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Nation API.
--
-- This module should be imported qualified, to prevent name clashes:
--
-- @
-- import NationStates
-- import qualified NationStates.Nation as Nation
-- @
--
-- In general, this module follows the terminology used in the
-- <https://www.nationstates.net/pages/api.html#nationapi official documentation>,
-- except when it clashes with Haskell keywords. For instance, the
-- @type@ shard has been renamed to 'type_'.
--
-- Here's a short example:
--
-- @
-- import NationStates
-- import qualified NationStates.Nation as Nation
-- import Text.Printf
--
-- main = 'NationStates.withContext' "ExampleBot/2000" $ \\c -> do
--     (name, motto) <- Nation.'run' "Montesardo-East Adanzi"
--         ((,) \<$\> Nation.'name' \<*\> Nation.'motto') c
--     printf "%s has the motto: %s\\n" name motto
-- @

module NationStates.Nation (

    -- * Running queries
    Nation(..),
    run,

    -- * Shards
    name,
    fullname,
    type_,
    motto,
    category,
    wa,
    endorsements,
    gavote,
    scvote,

    censusscore,
    censusscore',

    ) where


import Data.Maybe
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Text.XML.Light

import NationStates.Core


-- | A request to the Nation API.
newtype Nation a = Nation { unNation :: NS a }
    deriving (Functor, Applicative)


-- | Perform a request to the Nation API.
run
    :: String
        -- ^ Nation name
    -> Nation a
        -- ^ Requested shards
    -> Context
        -- ^ Connection manager
    -> IO a
run nation = requestNS (Just ("nation", nation)) . unNation


-- | Short name.
--
-- > "Testlandia"
name :: Nation String
name = Nation $ makeNS "name" Nothing "NAME"

-- | Full name, including pre-title.
--
-- > "The Republic of Testlandia"
fullname :: Nation String
fullname = Nation $ makeNS "fullname" Nothing "FULLNAME"

-- | Nation type.
--
-- > "Republic"
type_ :: Nation String
type_ = Nation $ makeNS "type" Nothing "TYPE"

-- | Motto.
--
-- > "It's a feature!"
motto :: Nation String
motto = Nation $ makeNS "motto" Nothing "MOTTO"

-- | Nation category.
--
-- > InoffensiveCentristDemocracy
category :: Nation WACategory
category = Nation . fmap parse $ makeNS "category" Nothing "CATEGORY"
  where
    parse = expect "category" readWACategory

-- | Whether the nation is in the World Assembly.
--
-- > True
wa :: Nation Bool
wa = Nation . fmap parse $ makeNS "wa" Nothing "UNSTATUS"
  where
    parse "WA Member" = True
    parse "Non-member" = False
    parse s = expected "WA status" s

-- | List of endorsements received.
--
-- > ["jlink","translenia","the_vines"]
endorsements :: Nation [String]
endorsements = Nation . fmap (splitDropBlanks ",") $
    makeNS "endorsements" Nothing "ENDORSEMENTS"

-- | General assembly vote.
--
-- > Just True
gavote :: Nation (Maybe Bool)
gavote = Nation . fmap (expect "General Assembly vote" readWAVote) $
    makeNS "gavote" Nothing "GAVOTE"

-- | Security council vote.
--
-- > Nothing
scvote :: Nation (Maybe Bool)
scvote = Nation . fmap (expect "Security Council vote" readWAVote) $
    makeNS "scvote" Nothing "SCVOTE"


-- | Query today's census.
--
-- Returns the current census ID, along with its value.
--
-- > (24,6.0)
censusscore :: Nation (Integer, Double)
censusscore = Nation $ makeNS' "censusscore" Nothing [] parse
  where
    parse q root = fromMaybe (error "could not find census score") $ do
        (i, _) <- MultiSet.minView $ MultiSet.difference response request
        x <- lookup i censusScores
        return (i, x)
      where
        censusScores = extractCensusScores root
        request = MultiSet.mapMaybe id . MultiSet.fromSet $
            queryShards q Map.! "censusscore"
        response = MultiSet.fromList $ map fst censusScores

-- | Query a census by its census ID.
--
-- > 94.0
censusscore' :: Integer -> Nation Double
censusscore' i = Nation $ makeNS' "censusscore" (Just i) [] parse
  where
    parse _ = fromMaybe (error $ "could not find census " ++ show i) .
        lookup i . extractCensusScores

extractCensusScores :: Element -> [(Integer, Double)]
extractCensusScores root = catMaybes [
    (,) <$> maybeId <*> maybeValue |
    Elem e <- elContent root,
    elName e == unqual "CENSUSSCORE",
    let maybeId = readMaybe =<< findAttr (unqual "id") e,
    let maybeValue = readMaybe $ strContent e ]
