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

  let annoyingText = genTextBy $ oneof [elements ['\n', 'r', '\t', ' ', ';', ',', '\\'], genValid]
  let forAllAnnoying :: (Testable prop) => (Text -> prop) -> Property
      forAllAnnoying = forAllShrink annoyingText shrinkValid
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
      forAllAnnoying $ \text ->
        unEscapeText (escapeText text) `shouldBe` text

  describe "assembleWithCommas" $ do
    let e l t =
          it (unwords ["can assemble", show l, "to", show t]) $
            assembleWithCommas l `shouldBe` t

    e [] ""
    e [""] ""
    e ["a"] "a"
    e ["a", "b"] "a,b"
    e [","] "\\,"
    e ["\n"] "\\n"

  describe "splitOnCommas" $ do
    let e t l =
          it (unwords ["can split", show t, "into", show l]) $
            splitOnCommas t `shouldBe` l

    e "" []
    e "a" ["a"]
    e "a,b" ["a", "b"]
    e "\\," [","]
    e "\\n" ["\n"]

    it "roundtrips with assembleWithCommas starting with assembling" $
      forAll (genValid `suchThat` (/= [""])) $ \l -> do
        let rendered = assembleWithCommas l
        context (show rendered) $ splitOnCommas rendered `shouldBe` l
    it "roundtrips with assembleWithCommas for annoying lists of text" $
      forAll (genListOf annoyingText `suchThat` (/= [""])) $ \l -> do
        let rendered = assembleWithCommas l
        context (show rendered) $ splitOnCommas rendered `shouldBe` l

  describe "splitOnSemicolons" $ do
    it "roundtrips with assembleWithSemicolons starting with assembling" $
      forAll (genValid `suchThat` (/= [""])) $ \l -> do
        let rendered = assembleWithSemicolons l
        context (show rendered) $ splitOnSemicolons rendered `shouldBe` l
    it "roundtrips with assembleWithSemicolons for annoying lists of text" $
      forAll (genListOf annoyingText `suchThat` (/= [""])) $ \l -> do
        let rendered = assembleWithSemicolons l
        context (show rendered) $ splitOnSemicolons rendered `shouldBe` l

  describe "splitOnSemicolonsThenCommas" $ do
    it "roundtrips with assembleWithCommasThenSemicolons" $
      forAll (genValid `suchThat` (\l -> l /= [[]] && notElem [""] l)) $ \l -> do
        let rendered = assembleWithCommasThenSemicolons l
        context (show rendered) $ splitOnSemicolonsThenCommas rendered `shouldBe` l
    it "roundtrips with assembleWithCommasThenSemicolons for annoying text" $
      forAll (genListOf (genListOf annoyingText) `suchThat` (\l -> l /= [[]] && notElem [""] l)) $ \l -> do
        let rendered = assembleWithCommasThenSemicolons l
        context (show rendered) $ splitOnSemicolonsThenCommas rendered `shouldBe` l

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
