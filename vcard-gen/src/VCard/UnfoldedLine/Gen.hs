{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.UnfoldedLine.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import VCard.UnfoldedLine

instance GenValid UnfoldedLine where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
