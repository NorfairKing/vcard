{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VCard.PropertyTypeSpec where

import Data.GenValidity.Text
import Data.Int
import Data.Text (Text)
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import VCard.ContentLine
import VCard.PropertyType
import VCard.PropertyType.Gen

spec :: Spec
spec = do
  describe "Text" $ do
    propertyTypeSpec @Text
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "Project XYZ Final Review\\nConference Room - 3B\\nCome Prepared.")
      ("Project XYZ Final Review\nConference Room - 3B\nCome Prepared." :: Text)

  let fancyCharacters = genTextBy $ elements ['\n', 'r', '\t', ' ', ';', ',', '\\']
  describe "escapeText" $ do
    it "escapes this diverse example correctly" $
      escapeText "hello world\n\\,;," `shouldBe` "hello world\\n\\\\\\,\\;\\,"

  describe "unEscapeText" $ do
    it "unEscapes this diverse example correctly" $
      unEscapeText "hello world\\n\\N\\\\\\,\\;\\," `shouldBe` "hello world\n\n\\,;,"

    it "roundtrips with escapeText" $
      forAllValid $ \text ->
        unEscapeText (escapeText text) `shouldBe` text

    it "roundtrips with escapeText for fancy characters" $
      forAll fancyCharacters $ \text ->
        unEscapeText (escapeText text) `shouldBe` text

  describe "URI" $ do
    genValidSpec @URI
    propertyTypeSpec @URI

    -- @
    -- Examples for "uri":
    --
    --     http://www.example.com/my/picture.jpg
    --     ldap://ldap.example.com/cn=babs%20jensen
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "http://www.example.com/my/picture.jpg")
      ("http://www.example.com/my/picture.jpg" :: URI)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "ldap://ldap.example.com/cn=babs%20jensen")
      ("ldap://ldap.example.com/cn=babs%20jensen" :: URI)

  describe "Integer" $ do
    propertyTypeSpec @Int64
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "4")
      (4 :: Int64)
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "+5")
      (5 :: Int64)
    propertyTypeRenderExampleSpec
      (mkSimpleContentLineValue "5")
      (5 :: Int64)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-6")
      (-6 :: Int64)

    -- [RFC 6350 Section 4.5](https://datatracker.ietf.org/doc/html/rfc6350#section-4.5)
    --
    -- @
    -- The maximum value is
    -- 9223372036854775807, and the minimum value is -9223372036854775808.
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "9223372036854775807")
      (9223372036854775807 :: Int64)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-9223372036854775808")
      (-9223372036854775808 :: Int64)

    -- [RFC 6350 Section 4.5](https://datatracker.ietf.org/doc/html/rfc6350#section-4.5)
    --
    -- @
    -- Examples:
    --
    --     1234567890
    --     -1234556790
    --     +1234556790,432109876
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "1234567890")
      (1234567890 :: Int64)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-1234567890")
      (-1234567890 :: Int64)
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "+1234567890")
      (1234567890 :: Int64)

  describe "Boolean" $ do
    propertyTypeSpec @Bool

    -- [RFC 6350 Section 4.4](https://datatracker.ietf.org/doc/html/rfc6350#section-4.4)
    --
    -- @
    -- Examples:
    --
    --     TRUE
    --     false
    --     True
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "TRUE")
      True
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "False")
      False
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "True")
      True
