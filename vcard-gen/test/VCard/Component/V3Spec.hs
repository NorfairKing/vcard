{-# LANGUAGE TypeApplications #-}

module VCard.Component.V3Spec (spec) where

import Conformance.TestUtils
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Gen

spec :: Spec
spec = do
  describe "Card" $ do
    genValidSpec @Card
    componentSpec @Card

  describe "renderCard" $
    it "roundtrips with parseCard" $
      forAllValid $ \vcard ->
        let rendered = renderCard vcard
            ctx = unlines ["Rendered VCARD:", T.unpack rendered]
         in context ctx $ do
              vcard' <- shouldConformStrict $ parseCard rendered
              vcard' `shouldBe` vcard
