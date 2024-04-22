{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Property.Gen where

import qualified Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Map ()
import Data.GenValidity.Text
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import VCard.Property

instance GenValid FormattedName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
