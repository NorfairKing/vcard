{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.PropertyType.URI
  ( URI (..),
    parseURI,
    renderURI,
  )
where

import Conformance
import Control.DeepSeq
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Validity.URI
import GHC.Generics (Generic)
import qualified Network.URI as Network
import VCard.ContentLine
import VCard.Parameter.ValueDataType
import VCard.PropertyType.Class

-- | URI
--
-- === [RFC 6350 section 4.2](https://datatracker.ietf.org/doc/html/rfc6350#section-4.2)
--
-- @

-- "uri": The "uri" value type should be used to identify values that
-- are referenced by a Uniform Resource Identifier (URI) instead of
-- encoded in-line.  These value references might be used if the value
-- is too large, or otherwise undesirable to include directly.  The
-- format for the URI is as defined in Section 3 of [RFC3986].  Note
-- that the value of a property of type "uri" is what the URI points to,
-- not the URI itself.
--
-- Examples for "uri":
--
--     http://www.example.com/my/picture.jpg
--     ldap://ldap.example.com/cn=babs%20jensen
-- @
newtype URI = URI {unURI :: Network.URI} -- Consider not making this a newtype.
  deriving (Show, Eq, Ord, Generic)

instance Validity URI

instance NFData URI

instance IsString URI where
  fromString s = case parseURI (fromString s) of
    Nothing -> error $ "unparseable URI in literal string: " <> s
    Just ca -> ca

instance IsPropertyType URI where
  propertyTypeValueType Proxy = TypeURI
  propertyTypeP clv = do
    let t = contentLineValueRaw clv
    case parseURI t of
      Just u -> pure u
      Nothing -> do
        case parseURI (unEscapeText t) of
          Nothing -> unfixableError $ UnparseableURI t
          Just uri -> do
            emitFixableError $ UrlTextEncoded t
            pure uri
  propertyTypeB = mkSimpleContentLineValue . renderURI

parseURI :: Text -> Maybe URI
parseURI = fmap URI . Network.parseURIReference . T.unpack

renderURI :: URI -> Text
renderURI = T.pack . dangerousURIToString . unURI
