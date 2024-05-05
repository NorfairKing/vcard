{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module VCard.PropertyType.Class
  ( PropertyTypeParseError (..),
    PropertyTypeParseFixableError (..),
    PropertyTypeParseWarning (..),
    IsPropertyType (..),

    -- * Helpers for defining IsPropertyType

    -- ** Parsers
    typedPropertyTypeP,
    propertyTypeListP,
    unEscapeText,
    splitOnCommas,
    splitOnSemicolons,
    splitOnSemicolonsThenCommas,
    assembleWithCommas,
    assembleWithSemicolons,
    assembleWithCommasThenSemicolons,

    -- ** Builders
    typedPropertyTypeB,
    propertyTypeListB,
    escapeText,
  )
where

import Conformance
import Control.Exception
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Int
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Data.Void
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
  | UnparseableBoolean !Text
  | UnparseableInteger !Text
  | UnparseableURI !Text
  | UnparseableTimestamp !Text
  deriving (Show, Eq, Ord)

instance Exception PropertyTypeParseError where
  displayException = \case
    UnexpectedValueType actual expected ->
      unlines
        [ "Uxpected value type.",
          unwords ["actual:   ", show actual],
          unwords ["expected: ", show expected]
        ]
    UnparseableBoolean t -> unwords ["Unparseable BOOLEAN", show t]
    UnparseableInteger t -> unwords ["Unparseable INTEGER", show t]
    UnparseableURI t -> unwords ["Unparseable URI", show t]
    UnparseableTimestamp t -> unwords ["Unparseable TIMESTAMP", show t]

data PropertyTypeParseFixableError
  = ParameterParseFixableError !ParameterParseFixableError
  | UrlTextEncoded !Text
  deriving (Show, Eq, Ord)

instance Exception PropertyTypeParseFixableError where
  displayException = \case
    ParameterParseFixableError ppfe -> displayException ppfe
    UrlTextEncoded t -> unwords ["URL was TEXT-encoded but should not have been:", show t]

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

-- [RFC 6350 Section 4.1](https://datatracker.ietf.org/doc/html/rfc6350#section-4.1)
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

instance IsPropertyType (CI Text) where
  propertyTypeValueType Proxy = TypeText
  propertyTypeP = fmap CI.mk . propertyTypeP
  propertyTypeB = propertyTypeB . CI.original

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

-- [RFC 6350 Section 4.4](https://datatracker.ietf.org/doc/html/rfc6350#section-4.4)
--
-- @
-- "boolean": The "boolean" value type is used to express boolean
-- values.  These values are case-insensitive.
--
-- Examples:
--
--     TRUE
--     false
--     True
-- @
instance IsPropertyType Bool where
  propertyTypeValueType Proxy = TypeBoolean
  propertyTypeP clv =
    let t = contentLineValueRaw clv
     in case CI.mk t of
          "TRUE" -> pure True
          "FALSE" -> pure False
          _ -> unfixableError $ UnparseableBoolean t
  propertyTypeB =
    mkSimpleContentLineValue . \case
      True -> "TRUE"
      False -> "FALSE"

-- | Integer
--
-- [RFC 6350 Section 4.5](https://datatracker.ietf.org/doc/html/rfc6350#section-4.5)
--
-- @
-- "integer": The "integer" value type is used to express signed
-- integers in decimal format.  If sign is not specified, the value is
-- assumed positive "+".  Multiple "integer" values can be specified
-- using the comma-separated notation.  The maximum value is
-- 9223372036854775807, and the minimum value is -9223372036854775808.
-- These limits correspond to a signed 64-bit integer using two's-
-- complement arithmetic.
--
-- Examples:
--
--     1234567890
--     -1234556790
--     +1234556790,432109876
-- @
instance IsPropertyType Int64 where
  propertyTypeValueType Proxy = TypeInteger
  propertyTypeP clv =
    let t = contentLineValueRaw clv
        s = T.unpack t
        go s' = case readMaybe s' of
          Nothing -> unfixableError $ UnparseableInteger t
          Just i -> pure i
     in case s of
          '+' : rest -> go rest
          _ -> go s
  propertyTypeB = mkSimpleContentLineValue . T.pack . show

-- | Timestamp
--
-- [RFC 6350 Section 4.3.5](https://datatracker.ietf.org/doc/html/rfc6350#section-4.3.5)
--
-- @
-- A complete date and time of day combination as specified in
-- [ISO.8601.2004], Section 4.3.2.
--
-- Examples for "timestamp":
--
--           19961022T140000
--           19961022T140000Z
--           19961022T140000-05
--           19961022T140000-0500
-- @
instance IsPropertyType UTCTime where
  propertyTypeValueType Proxy = TypeTimestamp
  propertyTypeP clv = do
    let t = contentLineValueRaw clv
    let s = T.unpack t
    let parses =
          [ formatParseM (utcTimeFormat (calendarFormat BasicFormat) (timeOfDayFormat BasicFormat)) s,
            localTimeToUTC utc <$> formatParseM (localTimeFormat (calendarFormat BasicFormat) (timeOfDayFormat BasicFormat)) s,
            zonedTimeToUTC <$> formatParseM (zonedTimeFormat (calendarFormat BasicFormat) (timeOfDayFormat BasicFormat) BasicFormat) s
          ]
    case msum parses of
      Nothing -> unfixableError $ UnparseableTimestamp t
      Just v -> pure v
  propertyTypeB = mkSimpleContentLineValue . T.pack . formatShow (utcTimeFormat (calendarFormat BasicFormat) (timeOfDayFormat BasicFormat))

typedPropertyTypeP ::
  forall propertyType.
  (IsPropertyType propertyType) =>
  ContentLineValue ->
  ConformPropertyType propertyType
typedPropertyTypeP clv = do
  mValueDataType <-
    conformMapAll absurd ParameterParseFixableError absurd $
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

propertyTypeListP ::
  (IsPropertyType propertyType) =>
  ContentLineValue ->
  ConformPropertyType [propertyType]
propertyTypeListP clv =
  if T.null (contentLineValueRaw clv)
    then pure []
    else
      let clvs = do
            raw <- splitOnCommas (contentLineValueRaw clv)
            pure (clv {contentLineValueRaw = raw})
       in mapM typedPropertyTypeP clvs

-- The plan:
-- Don't reuse splitOnCommas because they also need to un-escape backslashes.
-- in reassembleWithCommasThenSemicolons, don't reuse "reassembleWithCommas"
-- because they also need to escape backslashes.
splitOnSemicolonsThenCommas :: Text -> [[Text]]
splitOnSemicolonsThenCommas = map (map unEscapeText . splitOnUnescaped ',') . splitOnUnescaped ';'

-- Split on commas, but not escaped commas.
splitOnCommas :: Text -> [Text]
splitOnCommas = map unEscapeText . splitOnUnescaped ','

-- Split on semicolons, but not escaped semicolons.
splitOnSemicolons :: Text -> [Text]
splitOnSemicolons = map unEscapeText . splitOnUnescaped ';'

splitOnUnescaped :: Char -> Text -> [Text]
splitOnUnescaped s = \case
  "" -> []
  t -> map T.pack $ go [] $ T.unpack t
    where
      go :: String -> String -> [String]
      go acc = \case
        [] -> [reverse acc]
        '\\' : '\\' : rest -> go ('\\' : '\\' : acc) rest
        '\\' : c : rest | c == s -> go (c : '\\' : acc) rest
        c : rest
          | c == s -> reverse acc : go [] rest
          | otherwise -> go (c : acc) rest

assembleWithCommasThenSemicolons :: [[Text]] -> Text
assembleWithCommasThenSemicolons = T.intercalate ";" . map (T.intercalate "," . map escapeText)

assembleWithCommas :: [Text] -> Text
assembleWithCommas = assembleWith ','

assembleWithSemicolons :: [Text] -> Text
assembleWithSemicolons = assembleWith ';'

assembleWith :: Char -> [Text] -> Text
assembleWith c = T.intercalate (T.singleton c) . map escapeText

propertyTypeListB :: (IsPropertyType propertyType) => [propertyType] -> ContentLineValue
propertyTypeListB = \case
  [] -> emptyContentLineValue
  (pt : pts) ->
    let clv = propertyTypeB pt
        raw =
          T.intercalate "," $
            contentLineValueRaw clv : map (contentLineValueRaw . propertyTypeB) pts
     in clv {contentLineValueRaw = raw}
