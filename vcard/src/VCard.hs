{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module VCard
  ( -- * VCard
    VCard,
    Card (..),
    vcardContentType,
    isVCardExtension,

    -- ** Rendering
    renderVCardByteString,
    renderVCard,

    -- ** Parsing
    parseVCardByteString,
    parseVCard,

    -- *** Errors
    VCardParseError (..),
    VCardParseFixableError (..),
    VCardParseWarning (..),

    -- *** Running a 'Conform'
    runConformStrict,
    runConform,
    runConformLenient,
    runConformFlexible,
    module VCard,
    module VCard.UnfoldedLine,
  )
where

import Conformance
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import VCard.ContentLine
import VCard.Property
import VCard.UnfoldedLine

-- | MIME Content type
--
-- @
-- The text/vcard MIME content type (hereafter known as "vCard"; see
-- Section 10.1) contains contact information, typically pertaining to a
-- single contact or group of contacts.  The content consists of one or
-- more lines in the format given below.
-- @
--
-- @
-- The charset (see [RFC3536] for internationalization terminology) for
-- vCard is UTF-8 as defined in [RFC3629].  There is no way to override
-- this.  It is invalid to specify a value other than "UTF-8" in the
-- "charset" MIME parameter (see Section 10.1).
-- @
--
-- > vcardContentType = "text/calendar; charset=utf-8"
vcardContentType :: ByteString
vcardContentType = "text/vcard; charset=utf-8"

-- [Section 10.1](https://datatracker.ietf.org/doc/html/rfc6350#section-10.1)
-- > isVCardExtension ".vcf"
-- True
-- > isVCardExtension ".vcard"
-- True
isVCardExtension :: String -> Bool
isVCardExtension = (`elem` [".vcf", ".vcard"])

type VCard = [Card]

renderVCardByteString :: VCard -> ByteString
renderVCardByteString = TE.encodeUtf8 . renderVCard

renderVCard :: VCard -> Text
renderVCard =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . vcardB

vcardB :: VCard -> DList ContentLine
vcardB = foldMap cardB

parseVCardByteString ::
  ByteString ->
  Conform
    VCardParseError
    VCardParseFixableError
    VCardParseWarning
    VCard
parseVCardByteString = undefined

parseVCard ::
  Text ->
  Conform
    VCardParseError
    VCardParseFixableError
    VCardParseWarning
    VCard
parseVCard = undefined

type VCardParseError = Void

type VCardParseFixableError = Void

type VCardParseWarning = Void

data Card = Card
  { cardFormattedName :: !FormattedName
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

type CardParseError = Void

type CardParseFixableError = Void

type CardParseWarning = Void

cardB :: Card -> DList ContentLine
cardB = undefined

cardP :: Map ContentLineName (NonEmpty ContentLineValue) -> Conform CardParseError CardParseFixableError CardParseWarning Card
cardP = undefined
