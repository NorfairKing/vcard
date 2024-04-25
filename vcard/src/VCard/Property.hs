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

    -- ** Properties
    Begin (..),
    End (..),
    FormattedName (..),
    Version (..),
  )
where

import Conformance
import Control.DeepSeq
import Control.Exception
import Data.Proxy
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)
import VCard.ContentLine
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
  deriving (Show, Eq, Generic)

instance Exception PropertyParseFixableError where
  displayException = \case
    PropertyTypeParseFixableError ptpfe -> displayException ptpfe

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
  propertyType <- conformMapAll PropertyTypeParseError PropertyTypeParseFixableError id $ typedPropertyTypeP clv
  func propertyType

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
  { formattedNameValue :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity FormattedName

instance NFData FormattedName

instance IsProperty FormattedName where
  propertyName Proxy = "FN"
  propertyP = wrapPropertyTypeP FormattedName
  propertyB = propertyTypeB . formattedNameValue

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
