{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.DList (DList)
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
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

renderCard :: Card -> Text
renderCard =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . cardB

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
  ConformVCard VCard
parseVCard contents = do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  componentMap <- parseGeneralComponents contentLines
  if M.null componentMap
    then pure ([] :: VCard)
    else fmap concat $ forM (M.toList componentMap) $ \(componentName, components) ->
      case componentName of
        "VCARD" ->
          conformMapAll
            CardParseError
            CardParseFixableError
            absurd
            $ mapM cardP
            $ NE.toList components
        _ -> do
          emitWarning $ ComponentWarning $ UnknownComponent componentName
          pure []

parseCard ::
  Text ->
  ConformVCard Card
parseCard contents = do
  unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  (componentName, component) <- parseGeneralComponent contentLines
  case componentName of
    "VCARD" ->
      conformMapAll
        CardParseError
        CardParseFixableError
        absurd
        $ cardP component
    _ -> unfixableError $ ComponentNotCard componentName

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

data ComponentWarning = UnknownComponent !Text
  deriving (Show, Eq)

instance Exception ComponentWarning where
  displayException = \case
    UnknownComponent t ->
      unwords
        [ "Unknown component:",
          show t
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

requiredPropertyP :: forall a. (IsProperty a) => Map ContentLineName (NonEmpty ContentLineValue) -> ConformCard a
requiredPropertyP m = case M.lookup name m of
  Nothing -> unfixableError $ CardParseErrorMissingRequiredProperty name
  Just (value :| restValues) -> do
    case NE.nonEmpty restValues of
      Nothing -> pure ()
      Just (secondValue :| lastValues) ->
        emitFixableError $ MoreThanOneRequiredPropertyValue name value secondValue lastValues
    conformMapAll PropertyParseError PropertyParseFixableError absurd $ propertyContentLineP (ContentLine Nothing name value)
  where
    name = propertyName (Proxy :: Proxy a)

requiredPropertyB :: (IsProperty property) => property -> Map ContentLineName (NonEmpty ContentLineValue)
requiredPropertyB property =
  let cl = propertyContentLineB property
   in M.singleton (contentLineName cl) (contentLineValue cl :| [])

data VCardParseError
  = TextDecodingError !TE.UnicodeException
  | UnfoldingError !UnfoldingError
  | ContentLineParseError !String
  | ComponentParseError !ComponentParseError
  | ComponentNotCard !Text
  | CardParseError !CardParseError
  deriving (Show, Eq)

instance Exception VCardParseError where
  displayException = \case
    TextDecodingError e -> displayException e
    UnfoldingError ue -> displayException ue
    ContentLineParseError s -> s
    ComponentParseError cpe -> displayException cpe
    ComponentNotCard n -> unwords ["Component was not a VCARD:", show n]
    CardParseError cpe -> displayException cpe

data VCardParseFixableError
  = UnfoldingFixableError !UnfoldingFixableError
  | CardParseFixableError !CardParseFixableError
  deriving (Show, Eq)

instance Exception VCardParseFixableError where
  displayException = \case
    UnfoldingFixableError ue -> displayException ue
    CardParseFixableError cpfe -> displayException cpfe

data VCardParseWarning
  = ComponentWarning !ComponentWarning
  deriving (Show, Eq)

instance Exception VCardParseWarning where
  displayException = \case
    ComponentWarning cw -> displayException cw

type ConformVCard a = Conform VCardParseError VCardParseFixableError VCardParseWarning a

data Card = Card
  { cardFormattedName :: !FormattedName
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

data CardParseError
  = PropertyParseError !PropertyParseError
  | CardParseErrorMissingRequiredProperty !ContentLineName
  deriving (Show, Eq, Generic)

instance Exception CardParseError where
  displayException = \case
    PropertyParseError ppe -> displayException ppe
    CardParseErrorMissingRequiredProperty name ->
      unwords
        [ "Missing required property:",
          show (renderContentLineName name)
        ]

data CardParseFixableError
  = PropertyParseFixableError !PropertyParseFixableError
  | MoreThanOneRequiredPropertyValue !ContentLineName !ContentLineValue !ContentLineValue ![ContentLineValue]
  deriving (Show, Eq, Generic)

instance Exception CardParseFixableError where
  displayException = \case
    PropertyParseFixableError ppfe -> displayException ppfe
    MoreThanOneRequiredPropertyValue name v1 v2 vRest ->
      unlines $ unwords ["Multiple values of required property, guessing the first:", show name] : map show (v1 : v2 : vRest)

type CardParseWarning = Void

type ConformCard a = Conform CardParseError CardParseFixableError CardParseWarning a

cardB :: Card -> DList ContentLine
cardB Card {..} =
  mconcat
    [ DList.singleton $ propertyContentLineB $ Begin "VCARD",
      DList.singleton $ propertyContentLineB cardFormattedName,
      DList.singleton $ propertyContentLineB $ End "VCARD"
    ]

cardP :: Map ContentLineName (NonEmpty ContentLineValue) -> Conform CardParseError CardParseFixableError CardParseWarning Card
cardP componentProperties = do
  cardFormattedName <- requiredPropertyP componentProperties
  pure Card {..}
