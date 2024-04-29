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
import Path
import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Component.Class
import VCard.Component.V3 as V3
import VCard.Component.V4 as V4
import VCard.ContentLine
import VCard.ContentLine.Gen ()
import VCard.Property.Gen ()
import VCard.TestUtils

instance GenValid V3.Card where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid V4.Card where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AnyCard where
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
componentScenarioDir dir = do
  let parse bs = do
        textContents <- conformFromEither $ left TextDecodingError $ TE.decodeUtf8' bs
        parseComponentFromText textContents
  let parseStrictly = runConformStrict . parse
  let parseLeniently = runConformLenient . parse
  let renderBS =
        TE.encodeUtf8
          . renderUnfoldedLines
          . map renderContentLineToUnfoldedLine
          . DList.toList
          . namedComponentB
  vcardScenarioDirRecur (dir <> "/valid") $ \cardFile ->
    it "can parse this file as a component strictly and roundtrip it" $ do
      contents <- SB.readFile (fromRelFile cardFile)
      case parseStrictly contents of
        Left err -> expectationFailure $ renderError err
        Right result -> do
          shouldBeValid (result :: a)
          let rendered = renderBS result
          case parseStrictly rendered of
            Left err -> expectationFailure $ renderError err
            Right result' -> result' `shouldBe` result

  vcardScenarioDirRecur (dir <> "/fixable") $ \cardFile -> do
    it "fails to parse this file as a component strictly" $ do
      contents <- SB.readFile (fromRelFile cardFile)
      case parseStrictly contents of
        Left _ -> pure ()
        Right result -> expectationFailure $ unlines ["Expected to fail to parse, but got:", ppShow result]

    it "can parse this file as a component leniently and roundtrip it" $ do
      contents <- SB.readFile (fromRelFile cardFile)
      case parseLeniently contents of
        Left err -> expectationFailure $ displayException err
        Right (result, _) -> do
          shouldBeValid (result :: a)
          let rendered = renderBS result
          case parseStrictly rendered of
            Left err -> expectationFailure $ renderError err
            Right result' -> do
              result' `shouldBe` result
              goldenFile <- replaceExtension ".vcf-fixed" cardFile
              pure $ pureGoldenByteStringFile (fromRelFile goldenFile) rendered

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
