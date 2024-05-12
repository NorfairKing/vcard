{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.PropertyType.UTCOffset
  ( UTCOffset (..),
    parseUTCOffset,
    renderUTCOffset,

    -- * Computation
    utcOffsetTimeZone,

    -- * Helpers
    utcOffsetAbsBound,
  )
where

import Conformance
import Control.DeepSeq
import Data.Int
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Text.Read
import VCard.ContentLine
import VCard.Parameter
import VCard.PropertyType.Class

-- | UTC Offset
--
-- === [RFC 6350 Section 4.7](https://datatracker.ietf.org/doc/html/rfc6350#section-4.7)
--
-- @
-- "utc-offset": The "utc-offset" value type specifies that the property
-- value is a signed offset from UTC.  This value type can be specified
-- in the TZ property.
--
-- The value type is an offset from Coordinated Universal Time (UTC).
-- It is specified as a positive or negative difference in units of
-- hours and minutes (e.g., +hhmm).  The time is specified as a 24-hour
-- clock.  Hour values are from 00 to 23, and minute values are from 00
-- to 59.  Hour and minutes are 2 digits with high-order zeroes required
-- to maintain digit count.  The basic format for ISO 8601 UTC offsets
-- MUST be used.
-- @
newtype UTCOffset = UTCOffset {unUTCOffset :: Int16}
  deriving (Show, Eq, Ord, Generic)

instance Validity UTCOffset where
  validate uo@(UTCOffset offsetMinutes) =
    mconcat
      [ genericValidate uo,
        declare "the offset is in a sensible range" $
          -utcOffsetAbsBound < offsetMinutes && offsetMinutes < utcOffsetAbsBound
      ]

utcOffsetAbsBound :: Int16
utcOffsetAbsBound = (24 * 60) + 60

instance NFData UTCOffset

instance IsPropertyType UTCOffset where
  propertyTypeValueType Proxy = TypeUTCOffset
  propertyTypeP ContentLineValue {..} = do
    let t = contentLineValueRaw
    maybe (unfixableError $ UnparseableUTCOffset t) pure $ parseUTCOffset t
  propertyTypeB = mkSimpleContentLineValue . renderUTCOffset

parseUTCOffset :: Text -> Maybe UTCOffset
parseUTCOffset =
  ( \str ->
      let goOn r = case r of
            [h1, h2, m1, m2] -> do
              h <- readMaybe [h1, h2]
              m <- readMaybe [m1, m2]
              pure $ h * 60 + m
            _ -> Nothing
       in UTCOffset <$> case str of
            ('+' : rest) -> goOn rest
            ('-' : rest) -> negate <$> goOn rest
            _ -> goOn str
  )
    . T.unpack

renderUTCOffset :: UTCOffset -> Text
renderUTCOffset =
  T.pack
    . ( \i ->
          let sign = if i >= 0 then '+' else '-'
              a = abs i
              minutes = a `rem` 60
              zeroPad = \case
                [] -> "00"
                [c] -> ['0', c]
                s -> s
              minutesStr = zeroPad $ show minutes
              hours = a `div` 60
              hoursStr = zeroPad $ show hours
           in sign : hoursStr ++ minutesStr
      )
    . unUTCOffset

utcOffsetTimeZone :: UTCOffset -> Time.TimeZone
utcOffsetTimeZone (UTCOffset w) = Time.minutesToTimeZone $ fromIntegral w
