{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Property.Gen where

import Conformance
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.Syd
import Test.Syd.Validity
import VCard.ContentLine
import VCard.Parameter.Gen ()
import VCard.Property
import VCard.UnfoldedLine

instance GenValid Begin where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid End where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FormattedName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Version where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

propertyRenderExampleSpec ::
  ( Show property,
    IsProperty property,
    HasCallStack
  ) =>
  ContentLine ->
  property ->
  Spec
propertyRenderExampleSpec expected value =
  withFrozenCallStack $
    it "renders this example correctly" $
      context (show value) $
        let cl = propertyContentLineB value
         in context (T.unpack (renderUnfoldedLines [renderContentLineToUnfoldedLine cl])) $
              cl `shouldBe` expected

propertyParseExampleSpec ::
  ( IsProperty property,
    Show property,
    Eq property,
    HasCallStack
  ) =>
  ContentLine ->
  property ->
  Spec
propertyParseExampleSpec cl expected = withFrozenCallStack $
  it "parses this example correctly" $
    context (show cl) $
      case runConformStrict $ propertyContentLineP cl of
        Left err -> expectationFailure $ show err
        Right actual -> actual `shouldBe` expected

propertyExampleSpec ::
  ( IsProperty property,
    Show property,
    Eq property,
    HasCallStack
  ) =>
  ContentLine ->
  property ->
  Spec
propertyExampleSpec cl v = withFrozenCallStack $ do
  propertyParseExampleSpec cl v
  propertyRenderExampleSpec cl v

propertySpec ::
  forall a.
  (HasCallStack, Show a, Eq a, GenValid a, IsProperty a) =>
  Spec
propertySpec = withFrozenCallStack $ do
  it "always renders to a valid content line" $
    forAllValid $ \a ->
      shouldBeValid $ propertyContentLineB (a :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case runConformStrict $ propertyContentLineP (propertyContentLineB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through ContentLine" $
    forAllValid $ \a ->
      let rendered = propertyContentLineB (a :: a)
       in context (show rendered) $
            case runConformStrict $ propertyContentLineP rendered of
              Left err -> expectationFailure $ show err
              Right actual -> actual `shouldBe` a
