{-# LANGUAGE TypeApplications #-}

module VCard.V3Spec (spec) where

import Conformance.TestUtils
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Gen
import VCard.V3 as V3

spec :: Spec
spec = do
  describe "Card" $ do
    genValidSpec @V3.Card
    componentSpec @V3.Card

    componentScenarioDir @V3.Card "test_resources/v3"

  describe "renderCardV3" $
    it "roundtrips with parseCardV3" $
      forAllValid $ \vcard ->
        let rendered = renderCardV3 vcard
            ctx = unlines ["Rendered VCARD:", T.unpack rendered]
         in context ctx $ do
              vcard' <- shouldConformStrict $ parseCardV3 rendered
              vcard' `shouldBe` vcard

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
