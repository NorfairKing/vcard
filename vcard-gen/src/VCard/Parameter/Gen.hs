{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Parameter.Gen where

import Conformance
import Conformance.TestUtils
import Data.GenValidity
import Data.GenValidity.Text ()
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import VCard.ContentLine
import VCard.ContentLine.Gen ()
import VCard.Parameter

instance GenValid Language where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Preference where
  genValid = Preference <$> choose (1, 100)

instance GenValid AlternativeIdentifier where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

parameterExampleSpec ::
  (Show parameter, Eq parameter, IsParameter parameter, HasCallStack) =>
  ParamValue ->
  parameter ->
  Spec
parameterExampleSpec params val = withFrozenCallStack $ do
  parameterParseExampleSpec params val
  parameterRenderExampleSpec params val

parameterParseExampleSpec ::
  (Show parameter, Eq parameter, IsParameter parameter, HasCallStack) =>
  ParamValue ->
  parameter ->
  Spec
parameterParseExampleSpec params expected = withFrozenCallStack $ do
  it "parses this example correctly" $
    context (show params) $ do
      actual <- shouldConformStrict $ parameterP params
      actual `shouldBe` expected

parameterRenderExampleSpec ::
  (Show parameter, IsParameter parameter, HasCallStack) =>
  ParamValue ->
  parameter ->
  Spec
parameterRenderExampleSpec expected value = withFrozenCallStack $ do
  it "renders this example correctly" $
    context (show value) $
      let actual = parameterB value
       in actual `shouldBe` expected

parameterSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsParameter a) =>
  Spec
parameterSpec = do
  it "always renders a valid parameter values" $
    forAllValid $ \parameter ->
      shouldBeValid $ parameterB (parameter :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case runConformStrict $ parameterP (parameterB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through parameter values" $
    forAllValid $ \parameter -> do
      let values = parameterB (parameter :: a)
      actual <- shouldConformStrict $ parameterP values
      actual `shouldBe` parameter
