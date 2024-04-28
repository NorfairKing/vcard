{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VCard.Property
  ( -- * Property
    PropertyParseError,
    PropertyParseFixableError,
    PropertyParseWarning,
    ConformProperty,
    IsProperty (..),

    -- ** Parsing/Rendering content lines
    propertyContentLineP,
    propertyContentLineB,

    -- ** Helpers for implementing IsProperty
    wrapPropertyTypeP,
    viaPropertyTypeP,
    viaPropertyTypeListP,

    -- ** Properties

    -- *** Begin
    Begin (..),

    -- *** End
    End (..),

    -- *** Formatted name
    FormattedName (..),
    mkFormattedName,

    -- *** Name
    Name (..),
    mkName,

    -- *** Version
    Version (..),
  )
where

import Conformance
import Control.DeepSeq
import Control.Exception
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)
import VCard.ContentLine
import VCard.Parameter
import VCard.PropertyType

data PropertyParseError
  = PropertyTypeParseError !PropertyTypeParseError
  | MismatchedPropertyName
      -- Expected
      !ContentLineName
      -- Actual
      !ContentLineName
  deriving (Show, Eq, Generic)

instance Exception PropertyParseError where
  displayException = \case
    PropertyTypeParseError ptpe -> displayException ptpe
    MismatchedPropertyName expected actual ->
      unwords
        [ "Expected content line with name",
          show expected,
          "but got",
          show actual,
          "instead."
        ]

data PropertyParseFixableError
  = PropertyTypeParseFixableError !PropertyTypeParseFixableError
  | NameNotFiveComponents ![[Text]]
  deriving (Show, Eq, Generic)

instance Exception PropertyParseFixableError where
  displayException = \case
    PropertyTypeParseFixableError ptpfe -> displayException ptpfe
    NameNotFiveComponents namess -> unwords ["Expected exactly five lists of names, but found:", show namess]

type PropertyParseWarning = Void

type ConformProperty a = Conform PropertyParseError PropertyParseFixableError PropertyParseWarning a

-- | Calendar Properties
--
-- === [section 3.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7)
--
-- @
-- The Calendar Properties are attributes that apply to the iCalendar
-- object, as a whole.  These properties do not appear within a calendar
-- component.  They SHOULD be specified after the "BEGIN:VCALENDAR"
-- delimiter string and prior to any calendar component.
-- @
--
-- === Laws
--
-- * The 'ContentLineValue' that is built is valid:
--
-- >>> forAllValid $ \property -> isValid (propertyB property)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \contentLineValue -> isValid (propertyP contentlineValue)
--
-- * The property roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \property -> propertyP (propertyB property) == Right property
class IsProperty property where
  -- Name of the property
  propertyName :: Proxy property -> ContentLineName

  -- | Parser for the property
  propertyP :: ContentLineValue -> ConformProperty property

  -- | Builder for the property
  propertyB :: property -> ContentLineValue

propertyContentLineP ::
  forall property.
  (IsProperty property) =>
  ContentLine ->
  ConformProperty property
propertyContentLineP ContentLine {..} =
  let name = propertyName (Proxy :: Proxy property)
   in if contentLineName == name
        then propertyP contentLineValue
        else unfixableError $ MismatchedPropertyName name contentLineName

propertyContentLineB :: forall property. (IsProperty property) => property -> ContentLine
propertyContentLineB = ContentLine Nothing (propertyName (Proxy :: Proxy property)) . propertyB

wrapPropertyTypeP ::
  (IsPropertyType propertyType) =>
  (propertyType -> property) ->
  (ContentLineValue -> ConformProperty property)
wrapPropertyTypeP func = viaPropertyTypeP (pure . func)

viaPropertyTypeP ::
  forall propertyType property.
  (IsPropertyType propertyType) =>
  (propertyType -> ConformProperty property) ->
  (ContentLineValue -> ConformProperty property)
viaPropertyTypeP func clv = do
  propertyType <-
    conformMapAll PropertyTypeParseError PropertyTypeParseFixableError id $
      typedPropertyTypeP clv
  func propertyType

viaPropertyTypeListP ::
  forall propertyType property.
  (IsPropertyType propertyType) =>
  ([propertyType] -> ConformProperty property) ->
  (ContentLineValue -> ConformProperty property)
viaPropertyTypeListP func clv = do
  propertyType <-
    conformMapAll PropertyTypeParseError PropertyTypeParseFixableError id $
      propertyTypeListP clv
  func propertyType

propertyParamP ::
  (IsParameter param) =>
  ContentLineValue ->
  ConformProperty (Maybe param)
propertyParamP clv =
  conformMapAll (PropertyTypeParseError . ParameterParseError) (PropertyTypeParseFixableError . ParameterParseFixableError) id $
    optionalParam $
      contentLineValueParams clv

-- | BEGIN of a (VCARD) component
newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Begin

instance NFData Begin

instance IsProperty Begin where
  propertyName Proxy = "BEGIN"
  propertyP = wrapPropertyTypeP Begin
  propertyB = propertyTypeB . unBegin

-- | END of a (VCARD) component
newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity End

instance NFData End

instance IsProperty End where
  propertyName Proxy = "END"
  propertyP = wrapPropertyTypeP End
  propertyB = propertyTypeB . unEnd

-- [Section 6.2.1](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.1)
--
-- @
-- Purpose:  To specify the formatted text corresponding to the name of
--    the object the vCard represents.
--
-- Value type:  A single text value.
--
-- Cardinality:  1*
--
-- Special notes:  This property is based on the semantics of the X.520
--    Common Name attribute [CCITT.X520.1988].  The property MUST be
--    present in the vCard object.
--
-- ABNF:
--
--   FN-param = "VALUE=text" / type-param / language-param / altid-param
--            / pid-param / pref-param / any-param
--   FN-value = text
--
-- Example:
--
--       FN:Mr. John Q. Public\, Esq.
-- @
data FormattedName = FormattedName
  { formattedNameValue :: Text,
    formattedNameLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Generic)

instance Validity FormattedName

instance NFData FormattedName

instance IsProperty FormattedName where
  propertyName Proxy = "FN"
  propertyP clv = do
    formattedNameLanguage <- propertyParamP clv
    wrapPropertyTypeP (\formattedNameValue -> FormattedName {..}) clv
  propertyB FormattedName {..} =
    insertMParam formattedNameLanguage $
      propertyTypeB formattedNameValue

mkFormattedName :: Text -> FormattedName
mkFormattedName formattedNameValue =
  let formattedNameLanguage = Nothing
   in FormattedName {..}

-- [Section 6.2.2](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.2)
--
-- @
-- Purpose:  To specify the components of the name of the object the
--    vCard represents.
--
-- Value type:  A single structured text value.  Each component can have
--    multiple values.
--
-- Cardinality:  *1
--
-- Special note:  The structured property value corresponds, in
--    sequence, to the Family Names (also known as surnames), Given
--    Names, Additional Names, Honorific Prefixes, and Honorific
--    Suffixes.  The text components are separated by the SEMICOLON
--    character (U+003B).  Individual text components can include
--    multiple text values separated by the COMMA character (U+002C).
--    This property is based on the semantics of the X.520 individual
--    name attributes [CCITT.X520.1988].  The property SHOULD be present
--    in the vCard object when the name of the object the vCard
--    represents follows the X.520 model.
--
--    The SORT-AS parameter MAY be applied to this property.
--
-- ABNF:
--
--   N-param = "VALUE=text" / sort-as-param / language-param
--           / altid-param / any-param
--   N-value = list-component 4(";" list-component)
--
-- Examples:
--
--           N:Public;John;Quinlan;Mr.;Esq.
--
--           N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
-- @
data Name = Name
  { nameSurnames :: [Text],
    nameGivenNames :: [Text],
    nameAdditionalNames :: [Text],
    nameHonorificPrefixes :: [Text],
    nameHonorificSuffixes :: [Text],
    nameLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Generic)

instance Validity Name where
  validate n@Name {..} =
    let nonemptyNames f ns = decorate f $ decorateList ns $ \n' ->
          declare "The name is nonempty" $ not $ T.null n'
     in mconcat
          [ genericValidate n,
            nonemptyNames "nameSurnames" nameSurnames,
            nonemptyNames "nameGivenNames" nameGivenNames,
            nonemptyNames "nameAdditionalNames" nameAdditionalNames,
            nonemptyNames "nameHonorificPrefixes" nameHonorificPrefixes,
            nonemptyNames "nameHonorificSuffixes" nameHonorificSuffixes
          ]

instance NFData Name

instance IsProperty Name where
  propertyName Proxy = "N"
  propertyP clv = do
    nameLanguage <- propertyParamP clv
    let namess = map splitOnCommas (splitOnSemicolons (contentLineValueRaw clv))

    (nameSurnames, nameGivenNames, nameAdditionalNames, nameHonorificPrefixes, nameHonorificSuffixes) <- case namess of
      [n1, n2, n3, n4, n5] -> pure (n1, n2, n3, n4, n5)
      _ -> do
        emitFixableError $ NameNotFiveComponents namess
        pure $ case namess of
          (n1 : n2 : n3 : n4 : n5 : _) -> (n1, n2, n3, n4, n5)
          (n1 : n2 : n3 : n4 : _) -> (n1, n2, n3, n4, [])
          (n1 : n2 : n3 : _) -> (n1, n2, n3, [], [])
          (n1 : n2 : _) -> (n1, n2, [], [], [])
          (n1 : _) -> (n1, [], [], [], [])
          [] -> ([], [], [], [], [])
    pure Name {..}

  propertyB Name {..} =
    insertMParam nameLanguage $
      mkSimpleContentLineValue $
        T.intercalate
          ";"
          [ T.intercalate "," nameSurnames,
            T.intercalate "," nameGivenNames,
            T.intercalate "," nameAdditionalNames,
            T.intercalate "," nameHonorificPrefixes,
            T.intercalate "," nameHonorificSuffixes
          ]

mkName ::
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  Name
mkName nameSurnames nameGivenNames nameAdditionalNames nameHonorificPrefixes nameHonorificSuffixes =
  let nameLanguage = Nothing
   in Name {..}

-- [Section 6.7.9](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.9)
--
-- @
-- Purpose:  To specify the version of the vCard specification used to
--    format this vCard.
--
-- Value type:  A single text value.
--
-- Cardinality:  1
--
-- Special notes:  This property MUST be present in the vCard object,
--    and it must appear immediately after BEGIN:VCARD.  The value MUST
--    be "4.0" if the vCard corresponds to this specification.  Note
--    that earlier versions of vCard allowed this property to be placed
--    anywhere in the vCard object, or even to be absent.
--
-- ABNF:
--
--   VERSION-param = "VALUE=text" / any-param
--   VERSION-value = "4.0"
--
-- Example:
--
--         VERSION:4.0
-- @
newtype Version = Version
  { unVersion :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Version

instance NFData Version

instance IsProperty Version where
  propertyName Proxy = "VERSION"
  propertyP = wrapPropertyTypeP Version
  propertyB = propertyTypeB . unVersion
