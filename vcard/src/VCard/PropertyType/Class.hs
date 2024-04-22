{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module VCard.PropertyType.Class
  ( PropertyTypeParseError (..),
    PropertyTypeParseFixableError (..),
    IsPropertyType (..),

    -- * Helpers for defining IsPropertyType

    -- ** Parsers
    typedPropertyTypeP,

    -- ** Builders
    typedPropertyTypeB,
  )
where

import Conformance
import Control.Exception
import Data.CaseInsensitive (CI)
import Data.Int
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import Text.Megaparsec
import Text.Read
import VCard.ContentLine
import VCard.Parameter.Class
import VCard.Parameter.ValueDataType

data PropertyTypeParseError
  = ParameterParseError !ParameterParseError
  | UnexpectedValueType
      -- Actual
      !ValueDataType
      -- Expected
      !ValueDataType
  deriving (Show, Eq, Ord)

instance Exception PropertyTypeParseError where
  displayException = \case
    ParameterParseError ppe -> displayException ppe
    UnexpectedValueType actual expected ->
      unlines
        [ "Uxpected value type.",
          unwords ["actual:   ", show actual],
          unwords ["expected: ", show expected]
        ]

data PropertyTypeParseFixableError
  = ParameterParseFixableError !ParameterParseFixableError
  deriving (Show, Eq, Ord)

instance Exception PropertyTypeParseFixableError where
  displayException = \case
    ParameterParseFixableError ppfe -> displayException ppfe

type PropertyTypeParseWarning = Void

type ConformPropertyType a = Conform PropertyTypeParseError PropertyTypeParseFixableError PropertyTypeParseWarning a

-- | Property type
--
-- === [section 3.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3)
--
-- @
-- The properties in an iCalendar object are strongly typed.  The
-- definition of each property restricts the value to be one of the
-- value data types, or simply value types, defined in this section.
-- The value type for a property will either be specified implicitly as
-- the default value type or will be explicitly specified with the
-- "VALUE" parameter.  If the value type of a property is one of the
-- alternate valid types, then it MUST be explicitly specified with the
-- "VALUE" parameter.
-- @
--
-- === Laws
--
-- * The property roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \property -> propertyTypeP (propertyTypeB property) == Right property
class IsPropertyType propertyType where
  propertyTypeValueType :: Proxy propertyType -> ValueDataType

  -- | Parser for the property type
  propertyTypeP :: ContentLineValue -> ConformPropertyType propertyType

  -- | Builder for the property type
  propertyTypeB :: propertyType -> ContentLineValue

-- [Section 4.1](https://datatracker.ietf.org/doc/html/rfc6350#section-4.1)
--
-- @
-- "text": The "text" value type should be used to identify values that
-- contain human-readable text.  As for the language, it is controlled
-- by the LANGUAGE property parameter defined in Section 5.1.
--
-- Examples for "text":
--
--     this is a text value
--     this is one value,this is another
--     this is a single value\, with a comma encoded
--
-- A formatted text line break in a text value type MUST be represented
-- as the character sequence backslash (U+005C) followed by a Latin
-- small letter n (U+006E) or a Latin capital letter N (U+004E), that
-- is, "\n" or "\N".
--
-- For example, a multiple line NOTE value of:
--
--     Mythical Manager
--     Hyjinx Software Division
--     BabsCo, Inc.
--
-- could be represented as:
--
--     NOTE:Mythical Manager\nHyjinx Software Division\n
--      BabsCo\, Inc.\n
--
-- demonstrating the \n literal formatted line break technique, the
-- CRLF-followed-by-space line folding technique, and the backslash
-- escape technique.
-- @
instance IsPropertyType Text where
  propertyTypeValueType Proxy = TypeText
  propertyTypeP = pure . unEscapeText . contentLineValueRaw
  propertyTypeB = mkSimpleContentLineValue . escapeText

-- | Escape 'Text'
--
-- @
-- Some properties may contain one or more values delimited by a COMMA
-- character (U+002C).  Therefore, a COMMA character in a value MUST be
-- escaped with a BACKSLASH character (U+005C), even for properties that
-- don't allow multiple instances (for consistency).
-- @
--
-- @
-- Some properties (e.g., N and ADR) comprise multiple fields delimited
-- by a SEMICOLON character (U+003B).  Therefore, a SEMICOLON in a field
-- of such a "compound" property MUST be escaped with a BACKSLASH
-- character.  SEMICOLON characters in non-compound properties MAY be
-- escaped.  On input, an escaped SEMICOLON character is never a field
-- separator.  An unescaped SEMICOLON character may be a field
-- separator, depending on the property in which it appears.
-- @
--
-- @
-- Furthermore, some fields of compound properties may contain a list of
-- values delimited by a COMMA character.  Therefore, a COMMA character
-- in one of a field's values MUST be escaped with a BACKSLASH
-- character, even for fields that don't allow multiple values (for
-- consistency).  Compound properties allowing multiple instances MUST
-- NOT be encoded in a single content line.
-- @
--
-- @
-- Finally, BACKSLASH characters in values MUST be escaped with a
-- BACKSLASH character.  NEWLINE (U+000A) characters in values MUST be
-- encoded by two characters: a BACKSLASH followed by either an 'n'
-- (U+006E) or an 'N' (U+004E).
-- @
--
-- [Section 4.1](https://datatracker.ietf.org/doc/html/rfc6350#section-4.1)
--
-- @
-- A formatted text line break in a text value type MUST be represented
-- as the character sequence backslash (U+005C) followed by a Latin
-- small letter n (U+006E) or a Latin capital letter N (U+004E), that
-- is, "\n" or "\N".
-- @
escapeText :: Text -> Text
escapeText = T.concatMap go
  where
    -- FIXME this could probably go a LOT faster but we need to benchmark it
    go = \case
      '\n' -> "\\n"
      ',' -> "\\,"
      ';' -> "\\;"
      '\\' -> "\\\\"
      c -> T.singleton c

-- | Un-Escape 'Text'
--
--
-- Section [3.4](https://datatracker.ietf.org/doc/html/rfc6350#section-3.4)
--
-- @
-- Some properties may contain one or more values delimited by a COMMA
-- character (U+002C).  Therefore, a COMMA character in a value MUST be
-- escaped with a BACKSLASH character (U+005C), even for properties that
-- don't allow multiple instances (for consistency).
-- @
--
-- @
-- Some properties (e.g., N and ADR) comprise multiple fields delimited
-- by a SEMICOLON character (U+003B).  Therefore, a SEMICOLON in a field
-- of such a "compound" property MUST be escaped with a BACKSLASH
-- character.  SEMICOLON characters in non-compound properties MAY be
-- escaped.  On input, an escaped SEMICOLON character is never a field
-- separator.  An unescaped SEMICOLON character may be a field
-- separator, depending on the property in which it appears.
-- @
--
-- @
-- Furthermore, some fields of compound properties may contain a list of
-- values delimited by a COMMA character.  Therefore, a COMMA character
-- in one of a field's values MUST be escaped with a BACKSLASH
-- character, even for fields that don't allow multiple values (for
-- consistency).  Compound properties allowing multiple instances MUST
-- NOT be encoded in a single content line.
-- @
--
-- @
-- Finally, BACKSLASH characters in values MUST be escaped with a
-- BACKSLASH character.  NEWLINE (U+000A) characters in values MUST be
-- encoded by two characters: a BACKSLASH followed by either an 'n'
-- (U+006E) or an 'N' (U+004E).
-- @
--
-- [Section 4.1](https://datatracker.ietf.org/doc/html/rfc6350#section-4.1)
--
-- @
-- A formatted text line break in a text value type MUST be represented
-- as the character sequence backslash (U+005C) followed by a Latin
-- small letter n (U+006E) or a Latin capital letter N (U+004E), that
-- is, "\n" or "\N".
-- @
unEscapeText :: Text -> Text
unEscapeText = T.pack . go . T.unpack
  where
    -- FIXME this could probably go a LOT faster

    go = \case
      [] -> []
      '\\' : '\\' : rest -> '\\' : go rest
      '\\' : ',' : rest -> ',' : go rest
      '\\' : ';' : rest -> ';' : go rest
      '\\' : 'n' : rest -> '\n' : go rest
      '\\' : 'N' : rest -> '\n' : go rest
      c : rest -> c : go rest

typedPropertyTypeP ::
  forall propertyType.
  (IsPropertyType propertyType) =>
  ContentLineValue ->
  ConformPropertyType propertyType
typedPropertyTypeP clv = do
  mValueDataType <-
    conformMapAll ParameterParseError ParameterParseFixableError id $
      optionalParam (contentLineValueParams clv)
  let typ = propertyTypeValueType (Proxy :: Proxy propertyType)
  case mValueDataType of
    Just typ' ->
      if typ == typ'
        then pure ()
        else unfixableError $ UnexpectedValueType typ' typ
    _ -> pure ()
  propertyTypeP clv

typedPropertyTypeB ::
  forall propertyType.
  (IsPropertyType propertyType) =>
  propertyType ->
  ContentLineValue
typedPropertyTypeB =
  insertParam (propertyTypeValueType (Proxy :: Proxy propertyType))
    . propertyTypeB
