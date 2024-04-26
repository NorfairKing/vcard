{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    renderCard,

    -- ** Parsing
    parseVCardByteString,
    parseVCard,
    parseCard,

    -- *** Errors
    VCardParseError (..),
    VCardParseFixableError (..),
    VCardParseWarning (..),

    -- *** Helpers
    renderComponentText,
    parseComponentFromText,

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
import Control.Arrow (left)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Void
import VCard.Component
import VCard.ContentLine
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
vcardB = foldMap namedComponentB

renderCard :: Card -> Text
renderCard =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . namedComponentB

parseVCardByteString ::
  ByteString ->
  ConformVCard
    VCard
parseVCardByteString contents = do
  textContents <- conformFromEither $ left TextDecodingError $ TE.decodeUtf8' contents
  parseVCard textContents

parseVCard ::
  Text ->
  ConformVCard VCard
parseVCard contents = do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  componentMap <-
    conformMapAll ComponentParseError ComponentParseFixableError ComponentParseWarning $
      parseGeneralComponents contentLines
  if M.null componentMap
    then pure ([] :: VCard)
    else fmap concat $ forM (M.toList componentMap) $ \(name, components) ->
      conformMapAll
        ComponentParseError
        ComponentParseFixableError
        ComponentParseWarning
        $ mapM (namedComponentP name)
        $ NE.toList components

parseCard ::
  Text ->
  ConformVCard Card
parseCard contents = do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  (name, component) <-
    conformMapAll ComponentParseError ComponentParseFixableError ComponentParseWarning $
      parseGeneralComponent contentLines
  conformMapAll
    ComponentParseError
    ComponentParseFixableError
    ComponentParseWarning
    $ namedComponentP name component

data VCardParseError
  = TextDecodingError !TE.UnicodeException
  | UnfoldingError !UnfoldingError
  | ContentLineParseError !String
  | ComponentParseError !ComponentParseError
  | ComponentNotCard !Text
  deriving (Show, Eq)

instance Exception VCardParseError where
  displayException = \case
    TextDecodingError e -> displayException e
    UnfoldingError ue -> displayException ue
    ContentLineParseError s -> s
    ComponentParseError cpe -> displayException cpe
    ComponentNotCard n -> unwords ["Component was not a VCARD:", show n]

data VCardParseFixableError
  = UnfoldingFixableError !UnfoldingFixableError
  | ComponentParseFixableError !ComponentParseFixableError
  deriving (Show, Eq)

instance Exception VCardParseFixableError where
  displayException = \case
    UnfoldingFixableError ue -> displayException ue
    ComponentParseFixableError cpfe -> displayException cpfe

data VCardParseWarning
  = ComponentParseWarning !ComponentParseWarning
  deriving (Show, Eq)

instance Exception VCardParseWarning where
  displayException = \case
    ComponentParseWarning cw -> displayException cw

type ConformVCard a = Conform VCardParseError VCardParseFixableError VCardParseWarning a

-- | Render an individual component from Text directly
--
-- You probably don't want to use this.
-- Individual components are not described by the spec as text.
renderComponentText :: (IsComponent component) => component -> Text
renderComponentText =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . namedComponentB

-- | Parse an individual component from Text directly
--
-- You probably don't want to use this.
-- Individual components are not described by the spec as text.
parseComponentFromText ::
  (IsComponent component) =>
  Text ->
  ConformVCard component
parseComponentFromText contents = do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  conformMapAll ComponentParseError ComponentParseFixableError ComponentParseWarning $ parseComponentFromContentLines contentLines
