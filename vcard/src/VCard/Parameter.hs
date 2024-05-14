{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-exports #-}

module VCard.Parameter
  ( module VCard.Parameter,
    module VCard.Parameter.Class,
    module VCard.Parameter.ValueDataType,
  )
where

import Control.DeepSeq
import Data.Proxy
import Data.String
import qualified Data.Text as T
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Text.Read
import VCard.ContentLine
import VCard.Parameter.Class
import VCard.Parameter.ValueDataType

-- | Language
--
-- [RFC 6350 Section 5.1](https://datatracker.ietf.org/doc/html/rfc6350#section-5.1)
--
-- @
-- 5.1.  LANGUAGE
--
--    The LANGUAGE property parameter is used to identify data in multiple
--    languages.  There is no concept of "default" language, except as
--    specified by any "Content-Language" MIME header parameter that is
--    present [RFC3282].  The value of the LANGUAGE property parameter is a
--    language tag as defined in Section 2 of [RFC5646].
--
--    Examples:
--
--      ROLE;LANGUAGE=tr:hoca
--
--    ABNF:
--
--            language-param = "LANGUAGE=" Language-Tag
--              ; Language-Tag is defined in section 2.1 of RFC 5646
-- @
newtype Language =
  -- TODO consider actually implementing the tag from RFC5646 here.
  Language {unLanguage :: ParamValue}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity Language

instance NFData Language

instance IsParameter Language where
  parameterName Proxy = "LANGUAGE"
  parameterP = pure . Language
  parameterB = unLanguage

-- | Preference
--
-- [RFC 6350 Section 5.3](https://datatracker.ietf.org/doc/html/rfc6350#section-5.3)
--
-- @
-- The PREF parameter is OPTIONAL and is used to indicate that the
-- corresponding instance of a property is preferred by the vCard
-- author.  Its value MUST be an integer between 1 and 100 that
-- quantifies the level of preference.  Lower values correspond to a
-- higher level of preference, with 1 being most preferred.
--
-- When the parameter is absent, the default MUST be to interpret the
-- property instance as being least preferred.
--
-- Note that the value of this parameter is to be interpreted only in
-- relation to values assigned to other instances of the same property
-- in the same vCard.  A given value, or the absence of a value, MUST
-- NOT be interpreted on its own.
--
-- This parameter MAY be applied to any property that allows multiple
-- instances.
--
-- ABNF:
--
--         pref-param = "PREF=" (1*2DIGIT / "100")
--                              ; An integer between 1 and 100.
-- @
newtype Preference = Preference {unPreference :: Word8}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Preference where
  validate p@(Preference w) =
    mconcat
      [ genericValidate p,
        declare "The value is between 1 and and 100" $
          1 <= w && w <= 100
      ]

instance NFData Preference

instance IsParameter Preference where
  parameterName Proxy = "PREF"
  parameterP pv = case readMaybe $ T.unpack $ paramValueText pv of
    Nothing -> undefined
    Just w -> pure $ Preference w
  parameterB = UnquotedParam . T.pack . show . unPreference

-- | Preference
--
-- [RFC 6350 Section 5.4](https://datatracker.ietf.org/doc/html/rfc6350#section-5.4)
--
-- @
-- The ALTID parameter is used to "tag" property instances as being
-- alternative representations of the same logical property.  For
-- example, translations of a property in multiple languages generates
-- multiple property instances having different LANGUAGE (Section 5.1)
-- parameter that are tagged with the same ALTID value.
--
-- This parameter's value is treated as an opaque string.  Its sole
-- purpose is to be compared for equality against other ALTID parameter
-- values.
--
-- Two property instances are considered alternative representations of
-- the same logical property if and only if their names as well as the
-- value of their ALTID parameters are identical.  Property instances
-- without the ALTID parameter MUST NOT be considered an alternative
-- representation of any other property instance.  Values for the ALTID
-- parameter are not globally unique: they MAY be reused for different
-- property names.
--
-- Property instances having the same ALTID parameter value count as 1
-- toward cardinality.  Therefore, since N (Section 6.2.2) has
-- cardinality *1 and TITLE (Section 6.6.1) has cardinality *, these
-- three examples would be legal:
--
--   N;ALTID=1;LANGUAGE=jp:<U+5C71><U+7530>;<U+592A><U+90CE>;;;
--   N;ALTID=1;LANGUAGE=en:Yamada;Taro;;;
--   (<U+XXXX> denotes a UTF8-encoded Unicode character.)
--
--   TITLE;ALTID=1;LANGUAGE=fr:Patron
--   TITLE;ALTID=1;LANGUAGE=en:Boss
--
--   TITLE;ALTID=1;LANGUAGE=fr:Patron
--   TITLE;ALTID=1;LANGUAGE=en:Boss
--   TITLE;ALTID=2;LANGUAGE=en:Chief vCard Evangelist
--
-- while this one would not:
--
--   N;ALTID=1;LANGUAGE=jp:<U+5C71><U+7530>;<U+592A><U+90CE>;;;
--   N:Yamada;Taro;;;
--   (Two instances of the N property.)
--
-- and these three would be legal but questionable:
--
--   TITLE;ALTID=1;LANGUAGE=fr:Patron
--   TITLE;ALTID=2;LANGUAGE=en:Boss
--   (Should probably have the same ALTID value.)
--
--   TITLE;ALTID=1;LANGUAGE=fr:Patron
--   TITLE:LANGUAGE=en:Boss
--   (Second line should probably have ALTID=1.)
--
--   N;ALTID=1;LANGUAGE=jp:<U+5C71><U+7530>;<U+592A><U+90CE>;;;
--   N;ALTID=1;LANGUAGE=en:Yamada;Taro;;;
--   N;ALTID=1;LANGUAGE=en:Smith;John;;;
--   (The last line should probably have ALTID=2.  But that would be
--    illegal because N has cardinality *1.)
--
-- The ALTID property MAY also be used in may contexts other than with
-- the LANGUAGE parameter.  Here's an example with two representations
-- of the same photo in different file formats:
--
--   PHOTO;ALTID=1:data:image/jpeg;base64,...
--   PHOTO;ALTID=1;data:image/jp2;base64,...
--
-- ABNF:
--
--         altid-param = "ALTID=" param-value
-- @
newtype AlternativeIdentifier =
  -- TODO consider actually implementing the tag from RFC5646 here.
  AlternativeIdentifier {unAlternativeIdentifier :: ParamValue}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity AlternativeIdentifier

instance NFData AlternativeIdentifier

instance IsParameter AlternativeIdentifier where
  parameterName Proxy = "ALTID"
  parameterP = pure . AlternativeIdentifier
  parameterB = unAlternativeIdentifier
