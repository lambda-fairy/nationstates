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

    ) where


import NationStates.Core


newtype Nation a = Nation { unNation :: NS a }
    deriving (Functor, Applicative)


run :: String -> Nation a -> Manager -> IO a
run nation = requestNS (Just ("nation", nation)) . unNation


name :: Nation String
name = Nation $ simpleField "name" Nothing "NAME"

fullname :: Nation String
fullname = Nation $ simpleField "fullname" Nothing "FULLNAME"

type_ :: Nation String
type_ = Nation $ simpleField "type" Nothing "TYPE"

motto :: Nation String
motto = Nation $ simpleField "motto" Nothing "MOTTO"

category :: Nation WACategory
category = Nation . fmap parse $ simpleField "category" Nothing "CATEGORY"
  where
    parse = expect "category" readWACategory

wa :: Nation Bool
wa = Nation . fmap parse $ simpleField "wa" Nothing "UNSTATUS"
  where
    parse "WA Member" = True
    parse "Non-member" = False
    parse s = expected "WA status" s

endorsements :: Nation [String]
endorsements = Nation . fmap (splitDropBlanks ",") $
    simpleField "endorsements" Nothing "ENDORSEMENTS"
