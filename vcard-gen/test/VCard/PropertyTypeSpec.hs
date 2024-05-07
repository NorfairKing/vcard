{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VCard.PropertyTypeSpec where

import Data.GenValidity.Text
import Data.Int
import Data.Text (Text)
import Data.Time
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

  describe "Float" $ do
    -- Roundtrip doesn't work because NaNs don't equal themselves
    -- propertyTypeSpec @Double

    -- [RFC 6350 Section 4.6](https://datatracker.ietf.org/doc/html/rfc6350#section-4.6)
    --
    -- @
    -- Examples:
    --
    --     20.30
    --     1000000.0000001
    --     1.333,3.14
    -- @
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "20.30")
      (20.30 :: Double)
    propertyTypeRenderExampleSpec
      (mkSimpleContentLineValue "20.3")
      (20.30 :: Double)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "1000000.0000001")
      (1000000.0000001 :: Double)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "1.333")
      (1.333 :: Double)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "3.14")
      (3.14 :: Double)
    -- Big number, no scientific notation
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "1000000000000000000000.0")
      (1000000000000000000000.0 :: Double)

  describe "Date" $ do
    propertyTypeSpec @Day
    -- \| Date
    --
    -- [RFC 6350 Section 4.3.1](https://datatracker.ietf.org/doc/html/rfc6350#section-4.3.1)
    --
    -- @
    -- Examples for "date":
    --
    --           19850412
    --           1985-04
    --           1985
    --           --0412
    -- @
    propertyTypeExampleSpec (mkSimpleContentLineValue "19850412") (fromGregorian 1985 04 12)
  -- TODO
  -- propertyTypeExampleSpec "1985-04"
  -- propertyTypeExampleSpec "1985"
  -- propertyTypeExampleSpec "--0412"

  describe "Time" $ do
    propertyTypeSpec @TimeOfDay

    -- [RFC 6350 Section 4.3.5](https://datatracker.ietf.org/doc/html/rfc6350#section-4.3.5)
    --
    -- @
    -- Examples for "time":
    --
    --           102200
    --           1022
    --           10
    --           -2200
    --           --00
    --           102200Z
    --           102200-0800
    -- @
    propertyTypeExampleSpec (mkSimpleContentLineValue "102200") (TimeOfDay 10 22 00)
  -- TODO
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "102200")
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "1022")
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "10")
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "-2200")
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "--00")
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "102200Z")
  -- propertyTypeExampleSpec (mkSimpleContentLineValue "102200-0800")

  describe "Timestamp" $ do
    propertyTypeSpec @UTCTime

    -- [RFC 6350 Section 4.3.5](https://datatracker.ietf.org/doc/html/rfc6350#section-4.3.5)
    --
    -- @
    -- Examples for "timestamp":
    --
    --           19961022T140000
    --           19961022T140000Z
    --           19961022T140000-05
    --           19961022T140000-0500
    -- @
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19961022T140000")
      (UTCTime (fromGregorian 1996 10 22) (timeOfDayToTime (TimeOfDay 14 00 00)))
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "19961022T140000Z")
      (UTCTime (fromGregorian 1996 10 22) (timeOfDayToTime (TimeOfDay 14 00 00)))
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19961022T140000-05")
      (UTCTime (fromGregorian 1996 10 22) (timeOfDayToTime (TimeOfDay 19 00 00)))
    propertyTypeRenderExampleSpec
      (mkSimpleContentLineValue "19961022T190000Z")
      (UTCTime (fromGregorian 1996 10 22) (timeOfDayToTime (TimeOfDay 19 00 00)))
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19961022T140000-0500")
      (UTCTime (fromGregorian 1996 10 22) (timeOfDayToTime (TimeOfDay 19 00 00)))
