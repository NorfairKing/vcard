{-# LANGUAGE TypeApplications #-}

module VCard.V4Spec (spec) where

import Conformance.TestUtils
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Gen
import VCard.V4 as V4

spec :: Spec
spec = do
  describe "Card" $ do
    genValidSpec @V4.Card
    componentSpec @V4.Card

    componentScenarioDir @V4.Card "test_resources/v4"

  describe "renderCardV4" $
    it "roundtrips with parseCardV4" $
      forAllValid $ \vcard ->
        let rendered = renderCardV4 vcard
            ctx = unlines ["Rendered VCARD:", T.unpack rendered]
         in context ctx $ do
              vcard' <- shouldConformStrict $ parseCardV4 rendered
              vcard' `shouldBe` vcard

  describe "toV3" $ do
    it "produces valid V3 cards" $
      producesValid V4.toV3
    it "roundtrips with fromV3" $
      forAllValid $ \v3 -> do
        let rendered = V4.fromV3 v3
        let parsed = V4.toV3 rendered
        parsed `shouldBe` v3

  describe "fromV3" $ do
    it "produces valid V4 cards" $
      producesValid V4.fromV3
    it "roundtrips with toV3 back to v3" $
      forAllValid $ \v4 -> do
        let rendered = V4.toV3 v4
        let parsed = V4.fromV3 rendered
        V4.toV3 parsed `shouldBe` rendered

  describe "mergeCards" $ do
    it "produces valid cards" $
      producesValid2 mergeCards

    it "does nothing when merging with itself twice" $
      forAllValid $ \c -> do
        let mergedOnce = mergeCards c c
        mergeCards mergedOnce c `shouldBe` mergedOnce

    it "is idempotent" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 -> do
          let mergedOnce = mergeCards c1 c2
          mergeCards mergedOnce c2 `shouldBe` mergedOnce
