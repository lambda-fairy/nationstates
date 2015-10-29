-- | The Authentication API.
--
-- See <https://www.nationstates.net/pages/api.html#authentication>.
--
-- Here's a short example:
--
-- @
-- import NationStates
-- import qualified NationStates.Nation as Nation
-- import qualified NationStates.Verify as Nation
--
-- main = do
--     c <- 'NationStates.newContext' "ExampleBot/2000"
--     ok <- Nation.run "The Vines" (Nation.verify checksum token) c
--     putStrLn $ if ok then \"Success\" else \"Failure\"
--   where
--     checksum = \"CCT60sf2CfylDqSNzCMleqsvxrwjiG-9Zw4TXZjdMmk\"
--     token = Just "testing123"
-- @

module NationStates.Verify (
    verify,
    ) where


import qualified Data.Foldable as F
import Text.XML.Light

import NationStates.Core
import NationStates.Nation (Nation(..))


-- | Add an authentication token to an existing 'Nation' request.
verify
    :: String
        -- ^ Checksum
    -> Maybe String
        -- ^ Site-specific token
    -> Nation Bool
verify checksum token = Nation $ makeNS' query parse
  where
    query = withParams $
        [("a", "verify"), ("checksum", checksum)]
        ++ F.foldMap (\t -> [("token", t)]) token
    parse _ root
        | Just e <- findChild (unqual "VERIFY") root =
            case strContent e of
                "0" -> False
                "1" -> True
                s -> error $ "invalid value for <VERIFY>: " ++ show s
        | otherwise = error "missing <VERIFY> element"
