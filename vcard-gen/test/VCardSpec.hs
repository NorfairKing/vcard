{-# LANGUAGE TypeApplications #-}

module VCardSpec where

import Conformance
import Conformance.TestUtils
import Control.Exception
import qualified Data.ByteString as SB
import qualified Data.Text as T
import Path
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Gen
import VCard.TestUtils

spec :: Spec
spec = do
  genValidSpec @VCard
  genValidSpec @AnyCard
  componentSpec @AnyCard

  componentScenarioDir @AnyCard "test_resources/v3"
  componentScenarioDir @AnyCard "test_resources/v4"

  describe "vcardContentType" $
    it "is valid" $
      shouldBeValid vcardContentType

  vcardScenarioDirRecur "test_resources/vcard/valid" $ \cardFile ->
    it "parses this vcard strictly" $ do
      contents <- SB.readFile (fromRelFile cardFile)
      vcard <- shouldConformStrict $ parseVCardByteString contents
      shouldBeValid vcard

  vcardScenarioDirRecur "test_resources/vcard/fixable" $ \cardFile -> do
    it "cannot parse this vcard strictly" $ do
      contents <- SB.readFile (fromRelFile cardFile)
      case runConformStrict $ parseVCardByteString contents of
        Left _ -> pure ()
        Right vcard -> expectationFailure $ unlines ["Should have failed but succeeded in parsing this vcard:", ppShow vcard]

    it "can parse this vcard leniently and turn it into something valid that can be parsed strictly" $ do
      contents <- SB.readFile (fromRelFile cardFile)
      vcard <- shouldConformLenient $ parseVCardByteString contents
      shouldBeValid vcard
      let rendered = renderVCardByteString vcard
      vcard' <- shouldConformStrict $ parseVCardByteString rendered
      vcard' `shouldBe` vcard

  vcardScenarioDirRecur "test_resources/vcard/error" $ \cardFile -> do
    it "fails to parse this card" $ do
      contents <- SB.readFile $ fromRelFile cardFile
      err <- case runConform $ parseVCardByteString contents of
        Left err -> pure $ displayException err
        Right vcard -> expectationFailure $ unlines ["Should have failed but succeeded in parsing this vcard:", ppShow vcard]
      errorFile <- replaceExtension ".error" cardFile
      pure $ pureGoldenStringFile (fromRelFile errorFile) err

  describe "renderAnyCard" $
    it "roundtrips with parseAnyCard" $
      forAllValid $ \vcard ->
        let rendered = renderAnyCard vcard
            ctx = unlines ["Rendered VCARD:", T.unpack rendered]
         in context ctx $ do
              vcard' <- shouldConformStrict $ parseAnyCard rendered
              vcard' `shouldBe` vcard

  describe "anyCardToV3" $
    it "produces valid V3 cards" $
      producesValid anyCardToV3

  describe "anyCardToV4" $
    it "produces valid V4 cards" $
      producesValid anyCardToV4

  describe "renderVCard" $
    it "roundtrips with parseVCard" $
      forAllValid $ \vcard ->
        let rendered = renderVCard vcard
            ctx = unlines ["Rendered VCARD stream:", T.unpack rendered]
         in context ctx $ do
              vcard' <- shouldConformStrict $ parseVCard rendered
              vcard' `shouldBe` vcard

  describe "renderVCardByteString" $
    it "roundtrips with parseVCardByteString" $
      forAllValid $ \vcard -> do
        let rendered = renderVCardByteString vcard
        context (show rendered) $ do
          vcard' <- shouldConformStrict $ parseVCardByteString rendered
          vcard' `shouldBe` vcard
