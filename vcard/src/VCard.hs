{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Arrow (left)
import Control.DeepSeq
import Control.Exception
import Data.ByteString (ByteString)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
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
parseVCardByteString contents = do
  textContents <- conformFromEither $ left TextDecodingError $ TE.decodeUtf8' contents
  parseVCard textContents

parseVCard ::
  Text ->
  Conform
    VCardParseError
    VCardParseFixableError
    VCardParseWarning
    VCard
parseVCard contents = do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  conformMapAll CardParseError CardParseFixableError CardParseWarning $ undefined contentLines

type ComponentName = Text -- Only "VCARD" at the moment

type Component = Map ContentLineName (NonEmpty ContentLineValue)

data ComponentParseError
  = ComponentParseErrorMissingBegin
  | ComponentParseErrorMissingEnd !Text
  | ComponentParseErrorIncorrectEnd !Text !Text
  deriving (Show, Eq, Ord)

instance Exception ComponentParseError where
  displayException = \case
    ComponentParseErrorMissingBegin -> "Tried to parse a component, but didn't find a BEGIN property."
    ComponentParseErrorMissingEnd n ->
      unwords
        [ "Missing END property for component with name",
          show n
        ]
    ComponentParseErrorIncorrectEnd expected actual ->
      unwords
        [ unwords ["Missing END property for component with name", show expected],
          unwords ["found an END property for component with name", show actual, "instead."]
        ]

renderGeneralComponents :: Map ComponentName (NonEmpty Component) -> DList ContentLine
renderGeneralComponents =
  foldMap
    ( \(name, components) ->
        foldMap (renderGeneralComponent name) (NE.toList components)
    )
    . M.toList

renderGeneralComponent :: Text -> Component -> DList ContentLine
renderGeneralComponent name componentProperties =
  mconcat
    [ DList.singleton $ propertyContentLineB (Begin name),
      DList.fromList $
        concatMap
          ( \(n, values) ->
              map (ContentLine Nothing n) (NE.toList values)
          )
          (M.toList componentProperties),
      DList.singleton $ propertyContentLineB (End name)
    ]

parseGeneralComponents ::
  [ContentLine] ->
  ConformVCard
    (Map ComponentName (NonEmpty Component))
parseGeneralComponents = go
  where
    go ::
      [ContentLine] ->
      ConformVCard (Map ComponentName (NonEmpty Component))
    go = \case
      [] -> pure M.empty
      cls -> do
        ((name, component), leftovers) <- parseGeneralComponentHelper cls
        restComponents <- go leftovers
        pure (M.insertWith (<>) name (component :| []) restComponents)

parseGeneralComponent ::
  [ContentLine] ->
  ConformVCard (Text, Component)
parseGeneralComponent =
  -- TODO check that there were no other lines after this.
  fmap fst . parseGeneralComponentHelper

parseGeneralComponentHelper ::
  [ContentLine] ->
  ConformVCard
    ((ComponentName, Component), [ContentLine])
parseGeneralComponentHelper = \case
  [] -> unfixableError $ ComponentParseError ComponentParseErrorMissingBegin
  (firstCL : restCLs) -> do
    Begin name <-
      conformMapAll
        (CardParseError . PropertyParseError)
        (CardParseFixableError . PropertyParseFixableError)
        absurd
        $ propertyContentLineP firstCL
    go name M.empty M.empty restCLs
  where
    go ::
      Text ->
      Map ContentLineName (NonEmpty ContentLineValue) ->
      Map ComponentName (NonEmpty Component) ->
      [ContentLine] ->
      ConformVCard
        ((ComponentName, Component), [ContentLine])
    go name properties subComponents = \case
      [] -> unfixableError $ ComponentParseError $ ComponentParseErrorMissingEnd name
      (cl : rest) ->
        case contentLineName cl of
          "END" -> do
            End name' <-
              conformMapAll
                (CardParseError . PropertyParseError)
                (CardParseFixableError . PropertyParseFixableError)
                absurd
                $ propertyContentLineP
                  cl
            if name' == name
              then
                pure
                  ( ( name,
                      properties
                    ),
                    rest
                  )
              else unfixableError $ ComponentParseError $ ComponentParseErrorIncorrectEnd name name'
          "BEGIN" -> do
            ((name', subComponent), leftovers) <- parseGeneralComponentHelper (cl : rest)
            go
              name
              properties
              (M.insertWith (flip (<>)) name' (subComponent :| []) subComponents)
              leftovers
          _ ->
            go
              name
              (M.insertWith (flip (<>)) (contentLineName cl) (contentLineValue cl :| []) properties)
              subComponents
              rest

data VCardParseError
  = TextDecodingError !TE.UnicodeException
  | UnfoldingError !UnfoldingError
  | ContentLineParseError !String
  | ComponentParseError !ComponentParseError
  | CardParseError !CardParseError
  deriving (Show, Eq)

instance Exception VCardParseError where
  displayException = \case
    TextDecodingError e -> displayException e
    UnfoldingError ue -> displayException ue
    ContentLineParseError s -> s
    ComponentParseError cpe -> displayException cpe
    CardParseError cpe -> displayException cpe

data VCardParseFixableError
  = UnfoldingFixableError !UnfoldingFixableError
  | CardParseFixableError !CardParseFixableError
  deriving (Show, Eq)

instance Exception VCardParseFixableError where
  displayException = \case
    UnfoldingFixableError ue -> displayException ue
    CardParseFixableError cpfe -> displayException cpfe

data VCardParseWarning = CardParseWarning !CardParseWarning
  deriving (Show, Eq)

instance Exception VCardParseWarning where
  displayException = \case
    CardParseWarning cpw -> displayException cpw

type ConformVCard a = Conform VCardParseError VCardParseFixableError VCardParseWarning a

data Card = Card
  { cardFormattedName :: !FormattedName
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

data CardParseError = PropertyParseError !PropertyParseError
  deriving (Show, Eq, Generic)

instance Exception CardParseError where
  displayException = \case
    PropertyParseError ppe -> displayException ppe

data CardParseFixableError = PropertyParseFixableError !PropertyParseFixableError
  deriving (Show, Eq, Generic)

instance Exception CardParseFixableError where
  displayException = \case
    PropertyParseFixableError ppfe -> displayException ppfe

type CardParseWarning = Void

type ConformCard a = Conform CardParseError CardParseFixableError CardParseWarning a

cardB :: Card -> DList ContentLine
cardB = undefined

cardP :: Map ContentLineName (NonEmpty ContentLineValue) -> Conform CardParseError CardParseFixableError CardParseWarning Card
cardP = undefined
