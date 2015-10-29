{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- | Low-level tools for querying the NationStates API.
--
-- Most of the time, you should use the high-level wrappers in e.g.
-- "NationStates.Nation" instead. But if you need something not provided
-- by these wrappers, then feel free to use this module directly.

module NationStates.Core (

    -- * Requests
    NS,
    makeNS,
    makeNS',
    requestNS,
    apiVersion,

    -- * Query strings
    Query(..),
    shard,
    shard',
    withOptions,
    withParams,

    -- * Connection manager
    Context(..),

    -- * Utilities
    wordsBy,
    readMaybe,
    expect,
    pureIf,

    -- * Data structures
    module NationStates.Types,

    ) where


import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Functor.Compose
import qualified Data.Foldable as F
import Data.List
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
import Prelude  -- GHC 7.10

import NationStates.Types


-- | A request to the NationStates API.
--
-- * Construct an @NS@ using 'makeNS' or 'makeNS''.
-- * Compose @NS@ values using the 'Applicative' interface.
-- * Execute an @NS@ using 'requestNS'.
--
-- This type wraps a query string, along with a function that parses the
-- response. The funky type machinery keeps these two parts in sync, as
-- long as you stick to the 'Applicative' interface.
--
-- @
-- type NS a = ('Query', Query -> 'Element' -> a)
-- @
type NS = Compose ((,) Query) (Compose ((->) Query) ((->) Element))


-- | Construct a request for a single shard.
--
-- For example, this code requests the
-- <https://www.nationstates.net/cgi-bin/api.cgi?nation=testlandia&q=motto "motto">
-- shard:
--
-- @
-- motto :: NS String
-- motto = makeNS \"motto\" \"MOTTO\"
-- @
--
-- For more complex requests (e.g. nested elements), try 'makeNS'' instead.
makeNS
    :: String
        -- ^ Shard name
    -> String
        -- ^ XML element name
    -> NS String
makeNS shardName elemName = makeNS' (shard shardName) parse
  where
    parse _ = strContent . fromMaybe errorMissing . findChild (unqual elemName)
    errorMissing = error $ "missing <" ++ elemName ++ "> element"


-- | Construct a request.
makeNS'
    :: Query
        -- ^ Query string
    -> (Query -> Element -> a)
        -- ^ Function for parsing the response
    -> NS a
makeNS' query parse = Compose (query, Compose parse)


-- | Perform a request on the NationStates API.
requestNS
    :: Maybe (String, String)
        -- ^ Request type
    -> NS a
        -- ^ Set of shards to request
    -> Context
        -- ^ Connection manager
    -> IO a
requestNS kindAndName (Compose (q, Compose p)) c
    = parse . responseBody <$>
        (contextRateLimit c $ httpLbs req (contextManager c))
  where
    parse = p q . fromMaybe (error "invalid response") . parseXMLDoc
    req = initRequest {
        queryString = queryToString kindAndName q,
        requestHeaders
            = ("User-Agent", BC.pack $ contextUserAgent c)
            : requestHeaders initRequest,
        port = if contextIsSecure c then 443 else 80,
        secure = contextIsSecure c
        }

initRequest :: Request
Just initRequest = parseUrl "http://www.nationstates.net/cgi-bin/api.cgi"


-- | The version of the NationStates API used by this package.
--
-- Every request to NationStates includes this number. This means that
-- if the response format changes, existing code will continue to work
-- under the old API.
--
-- This number should match the current API version, as given by
-- <https://www.nationstates.net/cgi-bin/api.cgi?a=version>. If not,
-- please file an issue.
apiVersion :: Integer
apiVersion = 7


-- | Keeps track of rate limits and TLS connections.
--
-- You should create a single 'Context' at the start of your program,
-- then share it between multiple threads and requests.
data Context = Context {
    contextManager :: Manager,
    contextRateLimit :: forall a. IO a -> IO a,
    contextIsSecure :: Bool,
    contextUserAgent :: String
    }


-- | Keeps track of the set of shards to request.
data Query = Query {
    queryShards :: Map String (Set (Maybe Integer)),
    queryOptions :: Map String String,
    queryParams :: Map String String
    } deriving Show

instance Monoid Query where
    mempty = Query mempty mempty mempty
    mappend a b = Query {
        queryShards = Map.unionWith Set.union
            (queryShards a) (queryShards b),
        queryOptions = Map.unionWithKey mergeOptions
            (queryOptions a) (queryOptions b),
        queryParams = Map.unionWithKey mergeOptions
            (queryParams a) (queryParams b)
        }
      where
        mergeOptions key _ _
            = error $ "conflicting values for option " ++ show key


-- | Create a query for a single shard.
shard :: String -> Query
shard name = mempty { queryShards = Map.singleton name Set.empty }

-- | Create a query for a single shard, with an extra ID.
--
-- For example, the @censusscore-23@ shard would be written as:
-- @shard' "censusscore" 23@.
shard' :: String -> Integer -> Query
shard' name id' = mempty {
    queryShards = Map.singleton name (Set.singleton (Just id')) }

-- | Add extra @;@-delimited arguments.
withOptions :: [(String, String)] -> Query
withOptions options = mempty { queryOptions = Map.fromList options }

-- | Add extra @&@-delimited arguments.
withParams :: [(String, String)] -> Query
withParams params = mempty { queryParams = Map.fromList params }


queryToString :: Maybe (String, String) -> Query -> ByteString
queryToString kindAndName q
    = HTTP.renderQuery True (HTTP.toQuery $
        F.toList kindAndName
            ++ Map.toList (queryParams q)
            ++ [("q", shards), ("v", show apiVersion)])
    <> BC.pack options
  where
    shards
        | null (queryShards q) = "null"
        | otherwise
            = intercalate "+" [ name ++ F.foldMap (\i -> "-" ++ show i) maybeId |
                (name, is) <- Map.toList $ queryShards q,
                maybeId <- Set.toList is ]
    options = concat [ ";" ++ k ++ "=" ++ v |
        (k, v) <- Map.toList $ queryOptions q ]


-- | Split a list by the given predicate, dropping empty sublists.
--
-- >>> wordsBy (== ',') "the_vines,motesardo-east_adanzi,yellowapple"
-- ["the_vines", "montesardo-east_adanzi", "yellowapple"]
--
-- >>> wordsBy (== ',') ""
-- []
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
    [] -> []
    s' ->
        let (w, s'') = break p s'
        in  w : wordsBy p s''

-- | Parse an input string using the given parser function.
--
-- If parsing fails, raise an 'error'.
--
-- >>> (expect "integer" <*> readMaybe) "42" :: Integer
-- 42
--
-- >>> (expect "integer" <*> readMaybe) "butts" :: Integer
-- *** Exception: expected integer but got: butts
expect :: String -> String -> Maybe a -> a
expect want got = fromMaybe (error $ "expected " ++ want ++ " but got: " ++ got)

-- | Return the value only if the given predicate is true.
--
-- >>> pureIf (> 0) 5 :: Maybe Integer
-- Just 5
--
-- >>> pureIf (> 0) (-2) :: Maybe Integer
-- Nothing
pureIf :: Alternative f => (a -> Bool) -> a -> f a
pureIf p x = if p x then pure x else empty
