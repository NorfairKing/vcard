{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module VCard.ContentLineSpec where

import Conformance.TestUtils
import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Path
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec
import VCard.ContentLine
import VCard.ContentLine.Gen ()
import VCard.TestUtils
import VCard.UnfoldedLine

spec :: Spec
spec = do
  describe "escapeParamValue" $ do
    it "roundtrips with unescapeParamValue" $
      forAll (genTextBy (oneof [pure '^', genValid])) $ \t -> do
        let rendered = escapeParamValue t
        context (show rendered) $ unescapeParamValue rendered `shouldBe` t

  describe "ParamValue" $ do
    genValidSpec @ParamValue
    it "roundtrips ParamValues" $ parserBuilderRoundtrip paramValueP paramValueB

  describe "ParamName" $ do
    genValidSpec @ParamName
    it "roundtrips ParamNames" $ parserBuilderRoundtrip paramNameP paramNameB

  describe "ContentLineName" $ do
    genValidSpec @ContentLineName
    it "roundtrips ContentLineNames" $ parserBuilderRoundtrip contentLineNameP contentLineNameB

  describe "ContentLineValue" $ do
    genValidSpec @ContentLineValue
    it "roundtrips ContentLineValues" $ parserBuilderRoundtrip contentLineValueP contentLineValueB

  describe "ContentLine" $ do
    genValidSpec @ContentLine
    it "roundtrips ContentLines" $ parserBuilderRoundtrip contentLineP contentLineB

  describe "parseContentLineFromUnfoldedLine" $
    it "roundtrips with renderContentLineToUnfoldedLine" $
      forAllValid $ \contentLine ->
        case parseContentLineFromUnfoldedLine (renderContentLineToUnfoldedLine contentLine) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` contentLine

  describe "examples" $ do
    let examples :: [(ContentLine, Text)]
        examples = []
    forM_ examples $ \(contentLine, rendered) -> do
      it "renders this example correctly" $
        let actualRendered = renderContentLineToUnfoldedLine contentLine
         in case (,)
              <$> parseContentLineFromUnfoldedLine actualRendered
              <*> parseContentLineFromUnfoldedLine (UnfoldedLine rendered) of
              Left err -> expectationFailure err
              Right (actual, expected) -> actual `shouldBe` expected
      it "parses this example correctly" $
        case parseContentLineFromUnfoldedLine (UnfoldedLine rendered) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` contentLine

    -- Tests based on example calendars
    vcardScenarioDirRecur "test_resources/calendar/valid" $ \cardFile -> do
      it "can parse and unfold every line" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile cardFile)
        unfoldedLines <- shouldConformStrict $ parseUnfoldedLines contents
        case mapM parseContentLineFromUnfoldedLine unfoldedLines of
          Left err -> expectationFailure err
          Right contentLines -> shouldBeValid contentLines

    vcardScenarioDirRecur "test_resources/calendar/fixable" $ \cardFile -> do
      it "can parse and unfold every line" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile cardFile)
        unfoldedLines <- shouldConformLenient $ parseUnfoldedLines contents
        case mapM parseContentLineFromUnfoldedLine unfoldedLines of
          Left err -> expectationFailure err
          Right contentLines -> shouldBeValid contentLines

parseSucceedsSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseSucceedsSpec parser string expected =
  it "parses this value correctly" $
    case parse parser "test input" string of
      Left err -> expectationFailure $ errorBundlePretty err
      Right parsed -> parsed `shouldBe` expected

parserBuilderRoundtrip ::
  (Show a, Eq a, GenValid a) =>
  P a ->
  (a -> Text.Builder) ->
  Property
parserBuilderRoundtrip parser builder = forAllValid $ \a ->
  let rendered = LT.toStrict $ LTB.toLazyText $ builder a
   in context (T.unpack rendered) $
        case parse parser "test input" rendered of
          Left err -> expectationFailure $ errorBundlePretty err
          Right parsed -> parsed `shouldBe` a
