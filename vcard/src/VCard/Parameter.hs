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
