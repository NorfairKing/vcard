{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VCard.ComponentSpec where

import Conformance.TestUtils
import qualified Data.DList as DList
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity
import VCard.Component
import VCard.Component.Gen ()
import VCard.ContentLine.Gen

spec :: Spec
spec = do
  describe "Component" $ do
    genValidSpec @Component
    it "roundtrips through content lines" $
      forAllValid $ \name ->
        forAllValid $ \component -> do
          let rendered = DList.toList $ renderGeneralComponent name component
          context (T.unpack (renderContentLines rendered)) $ do
            parsed <- shouldConform $ parseGeneralComponent rendered
            parsed `shouldBe` (name, component)
