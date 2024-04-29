{-# LANGUAGE TypeApplications #-}

module VCard.Component.V4Spec (spec) where

import Conformance.TestUtils
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Gen
import VCard.Component.V4 as V4

spec :: Spec
spec = do
  describe "Card" $ do
    genValidSpec @V4.Card
    componentSpec @V4.Card

  describe "renderCard" $
    it "roundtrips with parseCard" $
      forAllValid $ \vcard ->
        let rendered = renderCardV4 vcard
            ctx = unlines ["Rendered VCARD:", T.unpack rendered]
         in context ctx $ do
              vcard' <- shouldConformStrict $ parseCardV4 rendered
              vcard' `shouldBe` vcard
