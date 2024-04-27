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
import Data.Validity
import GHC.Generics (Generic)
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
