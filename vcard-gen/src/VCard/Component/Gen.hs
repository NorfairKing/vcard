{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Component.Gen where

import Conformance
import Conformance.TestUtils
import Control.Arrow (left)
import Control.Exception
import qualified Data.ByteString as SB
import qualified Data.DList as DList
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Text.Encoding as TE
import Data.Void
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Class
import VCard.Component.V3 as V3
import VCard.Component.V4 as V4
import VCard.ContentLine
import VCard.ContentLine.Gen ()
import VCard.Property.Gen ()

instance GenValid V3.Card where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid V4.Card where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid Component" $
    forAllValid $ \component ->
      shouldBeValid $ DList.toList $ componentB (component :: a)

  it "roundtrips through Text" $
    forAllValid $ \a -> do
      let renderedText = renderComponentText (a :: a)
      parsed <- shouldConformStrict $ parseComponentFromText renderedText
      parsed `shouldBe` a

componentScenarioDir ::
  forall a.
  (Show a, Eq a, Validity a, IsComponent a) =>
  FilePath ->
  Spec
componentScenarioDir dir = scenarioDir dir $ \tzFile ->
  it "can parse this file as a component strictly and roundtrip it" $ do
    let parseBS bs = runConformStrict $ do
          textContents <- conformFromEither $ left TextDecodingError $ TE.decodeUtf8' bs
          unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines textContents
          contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
          conformMapAll ComponentParseError ComponentParseFixableError ComponentParseWarning $ parseComponentFromContentLines contentLines

        renderBS =
          TE.encodeUtf8
            . renderUnfoldedLines
            . map renderContentLineToUnfoldedLine
            . DList.toList
            . namedComponentB

    contents <- SB.readFile tzFile
    case parseBS contents of
      Left err -> expectationFailure $ renderError err
      Right result -> do
        shouldBeValid (result :: a)
        let rendered = renderBS result
        case parseBS rendered of
          Left err -> expectationFailure $ renderError err
          Right result' -> result' `shouldBe` result

renderError :: Either VCardParseError (Notes VCardParseFixableError VCardParseWarning) -> String
renderError = \case
  Left vcardParseError -> displayException vcardParseError
  Right (Notes fes wes) ->
    unlines $
      concat
        [ ["Fixable errors:"],
          map show fes,
          ["Warnings:"],
          map show wes
        ]
