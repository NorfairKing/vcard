{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.ContentLine.Gen where

import qualified Data.CaseInsensitive as CI
import Data.Char (chr)
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Map ()
import Data.GenValidity.Text
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import VCard.ContentLine
import VCard.UnfoldedLine

instance GenValid ContentLine where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContentLineName where
  genValid = ContentLineName . CI.mk <$> genNonEmptyTextBy genNameChar
  shrinkValid = shrinkValidStructurally

instance GenValid ContentLineGroup where
  genValid = ContentLineGroup . CI.mk <$> genNonEmptyTextBy genGroupChar
  shrinkValid = shrinkValidStructurally

instance GenValid ContentLineValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ParamName where
  genValid = ParamName . CI.mk <$> genNonEmptyTextBy genNameChar
  shrinkValid = shrinkValidStructurally

genNonEmptyTextBy :: Gen Char -> Gen Text
genNonEmptyTextBy gen = genTextBy gen `suchThat` (not . T.null)

genGroupChar :: Gen Char
genGroupChar = choose (chr 0, chr 127) `suchThat` (validationIsValid . validateGroupChar)

genNameChar :: Gen Char
genNameChar = choose (chr 0, chr 127) `suchThat` (validationIsValid . validateNameChar)

instance GenValid ParamValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genQSafeChar :: Gen Char
genQSafeChar = genValid `suchThat` (validationIsValid . validateQSafeChar)

genSafeChar :: Gen Char
genSafeChar = genValid `suchThat` (validationIsValid . validateSafeChar)

renderContentLines :: [ContentLine] -> Text
renderContentLines =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
