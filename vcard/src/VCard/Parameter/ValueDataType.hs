{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.Parameter.ValueDataType where

import Control.DeepSeq
import qualified Data.CaseInsensitive as CI
import Data.Proxy
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import VCard.ContentLine
import VCard.Parameter.Class

-- | Value Data Type
--
-- [section 5.2](https://datatracker.ietf.org/doc/html/rfc6350#section-5.2)
--
-- @
-- The VALUE parameter is OPTIONAL, used to identify the value type
-- (data type) and format of the value.  The use of these predefined
-- formats is encouraged even if the value parameter is not explicitly
-- used.  By defining a standard set of value types and their formats,
-- existing parsing and processing code can be leveraged.  The
-- predefined data type values MUST NOT be repeated in COMMA-separated
-- value lists except within the N, NICKNAME, ADR, and CATEGORIES
-- properties.
--
-- ABNF:
--
--   value-param = "VALUE=" value-type
--
--   value-type = "text"
--              / "uri"
--              / "date"
--              / "time"
--              / "date-time"
--              / "date-and-or-time"
--              / "timestamp"
--              / "boolean"
--              / "integer"
--              / "float"
--              / "utc-offset"
--              / "language-tag"
--              / iana-token  ; registered as described in section 12
--              / x-name
-- @
data ValueDataType
  = TypeText
  | TypeURI
  | TypeDate
  | TypeTime
  | TypeDateTime
  | TypeDateAndOrTime
  | TypeTimestamp
  | TypeBoolean
  | TypeInteger
  | TypeFloat
  | TypeUTCOffset
  | TypeLanguageTag
  | -- | Other value type
    --
    -- @
    -- Applications MUST preserve the value data for x-name and iana-
    -- token values that they don't recognize without attempting to
    -- interpret or parse the value data.
    -- @
    TypeOther !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ValueDataType

instance NFData ValueDataType

instance IsParameter ValueDataType where
  parameterName Proxy = "VALUE"
  parameterP =
    pure
      . ( \pv ->
            -- @
            -- Parameter values that are not explicitly defined as
            -- being case-sensitive are case-insensitive.
            -- @
            case CI.mk (paramValueText pv) of
              "text" -> TypeText
              "uri" -> TypeURI
              "date" -> TypeDate
              "time" -> TypeTime
              "date-time" -> TypeDateTime
              "date-and-or-ime" -> TypeDateAndOrTime
              "timestamp" -> TypeTimestamp
              "boolean" -> TypeBoolean
              "integer" -> TypeInteger
              "float" -> TypeFloat
              "utc-offset" -> TypeUTCOffset
              "language-tag" -> TypeLanguageTag
              _ -> TypeOther pv
        )
  parameterB = \case
    TypeText -> "text"
    TypeURI -> "uri"
    TypeDate -> "date"
    TypeTime -> "time"
    TypeDateTime -> "date-time"
    TypeDateAndOrTime -> "date-and-or-ime"
    TypeTimestamp -> "timestamp"
    TypeBoolean -> "boolean"
    TypeInteger -> "integer"
    TypeFloat -> "float"
    TypeUTCOffset -> "utc-offset"
    TypeLanguageTag -> "language-tag"
    TypeOther pv -> pv
