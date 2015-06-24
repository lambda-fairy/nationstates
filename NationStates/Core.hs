{-# LANGUAGE Rank2Types #-}

module NationStates.Core (

    NS,
    makeNS,
    requestNS,

    Context(..),

    Query(..),
    queryToUrl,

    simpleField,
    splitDropBlanks,

    readMaybe,
    expect,
    expected,

    module NationStates.Types,

    ) where


import qualified Data.ByteString.Char8 as BC
import Data.Functor.Compose
import Data.Foldable (toList)
import Data.List
import Data.List.Split
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Network.HTTP.Client
import qualified Network.HTTP.Types as HTTP
import Text.Read
import Text.XML.Light

import NationStates.Types


type NS = Compose ((,) Query) (Compose ((->) Query) ((->) Element))

-- | Construct a request for a single shard.
makeNS
    :: String
        -- ^ Shard name
    -> Maybe Integer
        -- ^ Shard ID
    -> [(String, String)]
        -- ^ List of options
    -> (Query -> Element -> a)
        -- ^ Function for parsing the response
    -> NS a
makeNS name maybeId options parse = Compose
    (Query {
        queryShards = Map.singleton name (Set.singleton maybeId),
        queryOptions = Map.fromList options
    }, Compose parse)


-- | Perform a request on the NationStates API.
requestNS
    :: Maybe (String, String)
        -- ^ Request type
    -> NS a
        -- ^ Set of shards to request
    -> Context
    -> IO a
requestNS kindAndName (Compose (q, Compose p)) c
    = parse . responseBody <$>
        (contextRateLimit c $ httpLbs req (contextManager c))
  where
    parse = p q . fromMaybe (error "invalid response") . parseXMLDoc
    req = initRequest {
        queryString
            = HTTP.renderQuery True (HTTP.toQuery $
                toList kindAndName ++ [("q", shards)])
            <> BC.pack options
        }
    (shards, options) = queryToUrl q

initRequest :: Request
Just initRequest = parseUrl "https://www.nationstates.net/cgi-bin/api.cgi"


-- | Keeps track of rate limits and TLS connections.
--
-- You should create a single 'Context' at the start of your program,
-- then share it between multiple threads and requests.
data Context = Context {
    contextManager :: Manager,
    contextRateLimit :: forall a. IO a -> IO a
    }


data Query = Query {
    queryShards :: Map String (Set (Maybe Integer)),
    queryOptions :: Map String String
    } deriving Show

instance Monoid Query where
    mempty = Query mempty mempty
    mappend a b = Query {
        queryShards = Map.unionWith Set.union
            (queryShards a) (queryShards b),
        queryOptions = Map.unionWithKey mergeOptions
            (queryOptions a) (queryOptions b)
        }
      where
        mergeOptions key _ _
            = error $ "conflicting values for option " ++ show key


queryToUrl :: Query -> (String, String)
queryToUrl q = (shards, options)
  where
    shards = intercalate "+" [ name ++ foldMap (\i -> "-" ++ show i) maybeId |
        (name, is) <- Map.toList $ queryShards q,
        maybeId <- Set.toList is ]
    options = concat [ ";" ++ k ++ "=" ++ v |
        (k, v) <- Map.toList $ queryOptions q ]


simpleField :: String -> Maybe Integer -> String -> NS String
simpleField shard maybeId elemName = makeNS shard maybeId [] parse
  where
    parse _ = strContent . fromMaybe errorMissing . findChild (unqual elemName)
    errorMissing = error $ "missing <" ++ elemName ++ "> element"

splitDropBlanks :: Eq a => [a] -> [a] -> [[a]]
splitDropBlanks = split . dropBlanks . dropDelims . onSublist


expect :: String -> (String -> Maybe a) -> String -> a
expect want parse = fromMaybe <$> expected want <*> parse

expected :: String -> String -> a
expected want s = error $ "invalid " ++ want ++ ": " ++ show s
