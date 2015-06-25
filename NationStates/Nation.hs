{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NationStates.Nation (

    Nation(..),
    run,

    name,
    fullname,
    wa,
    type_,
    motto,
    category,
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


newtype Nation a = Nation { unNation :: NS a }
    deriving (Functor, Applicative)


run :: String -> Nation a -> Context -> IO a
run nation = requestNS (Just ("nation", nation)) . unNation


name :: Nation String
name = Nation $ makeNS "name" Nothing "NAME"

fullname :: Nation String
fullname = Nation $ makeNS "fullname" Nothing "FULLNAME"

type_ :: Nation String
type_ = Nation $ makeNS "type" Nothing "TYPE"

motto :: Nation String
motto = Nation $ makeNS "motto" Nothing "MOTTO"

category :: Nation WACategory
category = Nation . fmap parse $ makeNS "category" Nothing "CATEGORY"
  where
    parse = expect "category" readWACategory

wa :: Nation Bool
wa = Nation . fmap parse $ makeNS "wa" Nothing "UNSTATUS"
  where
    parse "WA Member" = True
    parse "Non-member" = False
    parse s = expected "WA status" s

endorsements :: Nation [String]
endorsements = Nation . fmap (splitDropBlanks ",") $
    makeNS "endorsements" Nothing "ENDORSEMENTS"

gavote :: Nation (Maybe Bool)
gavote = Nation . fmap (expect "General Assembly vote" readWAVote) $
    makeNS "gavote" Nothing "GAVOTE"

scvote :: Nation (Maybe Bool)
scvote = Nation . fmap (expect "Security Council vote" readWAVote) $
    makeNS "scvote" Nothing "SCVOTE"


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
