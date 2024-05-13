{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module VCard.PropertyType.LanguageTag
  ( LanguageTag (..),
  )
where

import Control.DeepSeq
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import VCard.ContentLine
import VCard.Parameter.ValueDataType
import VCard.PropertyType.Class

-- | Language Tag
--
-- === [RFC 6350 section 4.8](https://datatracker.ietf.org/doc/html/rfc6350#section-4.8)
--
-- @
-- "language-tag": A single language tag, as defined in [RFC5646].
-- @
newtype LanguageTag = LanguageTag {unLanguageTag :: Text}
  deriving (Show, Eq, Ord, Generic, IsString)

instance Validity LanguageTag

instance NFData LanguageTag

instance IsPropertyType LanguageTag where
  propertyTypeValueType Proxy = TypeLanguageTag
  propertyTypeP clv = do
    let t = contentLineValueRaw clv
    pure $ LanguageTag t
  propertyTypeB = mkSimpleContentLineValue . unLanguageTag
