{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module VCard
  ( -- * VCard
    VCard,
    AnyCard (..),
    V4.Card (..),
    vcardContentType,
    isVCardExtension,

    -- ** Rendering
    renderVCardByteString,
    renderVCard,
    renderAnyCard,
    renderCardV4,
    renderCardV3,

    -- ** Parsing
    parseVCardByteString,
    parseVCard,
    parseAnyCard,
    parseCardV4,
    parseCardV3,

    -- *** Errors
    VCardParseError (..),
    VCardParseFixableError (..),
    VCardParseWarning (..),

    -- *** Helpers
    renderComponentText,
    parseComponentFromText,
    withLineEndingsFixableError,

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
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import VCard.Component
import VCard.ContentLine
import VCard.Property
import VCard.UnfoldedLine
import VCard.V3 as V3
import VCard.V4 as V4

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

type VCard = [AnyCard]

data AnyCard
  = CardV4 !V4.Card
  | CardV3 !V3.Card
  deriving (Show, Eq, Generic)

instance Validity AnyCard

instance NFData AnyCard

instance IsComponent AnyCard where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    version <- requiredPropertyP componentProperties
    case unVersion version of
      "4.0" -> CardV4 <$> componentP componentProperties
      "3.0" -> CardV3 <$> componentP componentProperties
      _ -> unfixableError $ ComponentParseErrorUnknownVersion version
  componentB = \case
    CardV4 c -> componentB c
    CardV3 c -> componentB c

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

renderAnyCard :: AnyCard -> Text
renderAnyCard = \case
  CardV4 c -> renderCardV4 c
  CardV3 c -> renderCardV3 c

renderCardV3 :: V3.Card -> Text
renderCardV3 = renderComponentText

renderCardV4 :: V4.Card -> Text
renderCardV4 = renderComponentText

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
parseVCard = withLineEndingsFixableError $ \contents -> do
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

withLineEndingsFixableError :: (Text -> ConformVCard component) -> Text -> ConformVCard component
withLineEndingsFixableError parseContents rawContents = do
  -- Sometimes implementations use LF instead of CRLF line endings, see
  -- https://github.com/pimutils/vdirsyncer/issues/1128
  -- In such a case we will get a ComponentParseErrorMissingEnd error with the entire file in the name.
  -- We match on this case and try to parse again with line endings "fixed".
  errOrVCard <- tryConformDetailed (parseContents rawContents)
  let fixedContents = T.replace "\n" "\r\n" rawContents
  case errOrVCard of
    Left hr -> case hr of
      HaltedBecauseOfUnfixableError (ComponentParseError (ComponentParseErrorMissingEnd name))
        | T.isSuffixOf "\nEND:VCARD" (T.toUpper (CI.original name)) -> do
            -- TODO throw fixable error
            parseContents fixedContents
      HaltedBecauseOfUnfixableError ue -> unfixableError ue
      HaltedBecauseOfStrictness fe -> do
        emitFixableError fe
        parseContents fixedContents -- Won't get here because we use the same predicate
    Right vcard -> do
      pure vcard

parseAnyCard :: Text -> ConformVCard AnyCard
parseAnyCard = parseComponentFromText

parseCardV4 :: Text -> ConformVCard V4.Card
parseCardV4 = parseComponentFromText

parseCardV3 :: Text -> ConformVCard V3.Card
parseCardV3 = parseComponentFromText

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
parseComponentFromText = withLineEndingsFixableError $ \contents -> do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  conformMapAll ComponentParseError ComponentParseFixableError ComponentParseWarning $ parseComponentFromContentLines contentLines
