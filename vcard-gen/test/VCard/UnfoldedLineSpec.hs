{-# LANGUAGE OverloadedStrings #-}

module VCard.UnfoldedLineSpec where

import Conformance
import Conformance.TestUtils
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import Path
import Test.Syd
import Test.Syd.Validity
import VCard.TestUtils
import VCard.UnfoldedLine
import VCard.UnfoldedLine.Gen ()

spec :: Spec
spec = do
  describe "parseUnfoldedLines" $ do
    it "parses empty text as no lines" $
      shouldConformStrict (parseUnfoldedLines "") `shouldReturn` []
    it "parses an empty line as such" $
      shouldConformStrict (parseUnfoldedLines "\r\n") `shouldReturn` [""]
    it "roundtrips with renderUnfoldedLinesText" $
      forAllValid $ \unfoldedLines ->
        shouldConformStrict (parseUnfoldedLines (renderUnfoldedLines unfoldedLines))
          `shouldReturn` unfoldedLines

    -- Test based on this part of the spec:
    --
    -- [Section 3.2](https://datatracker.ietf.org/doc/html/rfc6350#section-3.2)
    -- @
    -- For example, the line:
    --
    --   DESCRIPTION:This is a long description that exists on a long line.
    --
    -- Can be represented as:
    --
    --   DESCRIPTION:This is a lo
    --    ng description
    --     that exists on a long line.
    -- @
    it "parses these two into the same unfolded lines:" $ do
      let line1 = "DESCRIPTION:This is a long description that exists on a long line.\r\n"
      let line2 = "DESCRIPTION:This is a lo\r\n ng description\r\n  that exists on a long line.\r\n"
      runConformStrict (parseUnfoldedLines line1) `shouldBe` runConformStrict (parseUnfoldedLines line2)
  describe "renderUnfoldedLinesText" $ do
    it "renders no lines as nothing" $
      renderUnfoldedLines [] `shouldBe` ""
    it "renders an empty line as such" $
      renderUnfoldedLines [""] `shouldBe` "\r\n"
    it "produces the same folded line as before" $
      pureGoldenTextFile
        "test_resources/unfolded-lines.txt"
        ( renderUnfoldedLines
            [ "", -- empty line
              "This is rather a short line.",
              "This is a very long line which definitely contains more than seventy five characters.",
              "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
            ]
        )

  -- Test based on this coment in the spec:
  --
  -- @
  -- Note: It is possible for very simple implementations to generate
  -- improperly folded lines in the middle of a UTF-8 multi-octet
  -- sequence.  For this reason, implementations need to unfold lines
  -- in such a way to properly restore the original sequence.
  -- @
  pending "that multi-octet UTF-8 sequences are preserved correctly"

  -- Tests based on example calendars
  vcardScenarioDirRecur "test_resources/vcard/valid" $ \cardFile -> do
    it "can parse and unfold every line" $ do
      contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile cardFile)
      unfoldedLines <- shouldConformStrict (parseUnfoldedLines contents)
      shouldBeValid unfoldedLines

  vcardScenarioDirRecur "test_resources/vcard/fixable" $ \cardFile -> do
    it "can parse and unfold every line" $ do
      contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile cardFile)
      unfoldedLines <- shouldConformLenient (parseUnfoldedLines contents)
      shouldBeValid unfoldedLines
