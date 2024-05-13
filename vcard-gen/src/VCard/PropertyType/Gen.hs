{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.PropertyType.Gen where

import Conformance
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.URI ()
import GHC.Stack
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import VCard.ContentLine
import VCard.Parameter ()
import VCard.Parameter.Gen ()
import VCard.PropertyType

instance GenValid URI where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UTCOffset where
  genValid = UTCOffset <$> choose (-utcOffsetAbsBound, utcOffsetAbsBound)

instance GenValid LanguageTag where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

propertyTypeRenderExampleSpec ::
  ( Show propertyType,
    IsPropertyType propertyType,
    HasCallStack
  ) =>
  ContentLineValue ->
  propertyType ->
  Spec
propertyTypeRenderExampleSpec expected value =
  withFrozenCallStack $
    it "renders this example correctly" $
      context (show value) $
        propertyTypeB value `shouldBe` expected

propertyTypeParseExampleSpec ::
  ( Show propertyType,
    Eq propertyType,
    IsPropertyType propertyType,
    HasCallStack
  ) =>
  ContentLineValue ->
  propertyType ->
  Spec
propertyTypeParseExampleSpec clv expected = withFrozenCallStack $
  it "parses this example correctly" $
    context (show clv) $
      case runConformStrict $ propertyTypeP clv of
        Left err -> expectationFailure $ show err
        Right actual -> actual `shouldBe` expected

propertyTypeExampleSpec ::
  ( Show propertyType,
    IsPropertyType propertyType,
    Eq propertyType,
    HasCallStack
  ) =>
  ContentLineValue ->
  propertyType ->
  Spec
propertyTypeExampleSpec clv value = withFrozenCallStack $ do
  propertyTypeParseExampleSpec clv value
  propertyTypeRenderExampleSpec clv value

propertyTypeSpec ::
  forall a.
  (HasCallStack, Show a, Eq a, GenValid a, IsPropertyType a) =>
  Spec
propertyTypeSpec = withFrozenCallStack $ do
  it "always renders to a valid content line" $
    forAllValid $ \propertyType ->
      shouldBeValid $ propertyTypeB (propertyType :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case runConformStrict $ propertyTypeP (propertyTypeB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through ContentLine" $
    forAllValid $ \propertyType ->
      let value = propertyTypeB (propertyType :: a)
       in context (show value) $
            case runConformStrict $ propertyTypeP value of
              Left err -> expectationFailure $ show err
              Right actual -> actual `shouldBe` propertyType
