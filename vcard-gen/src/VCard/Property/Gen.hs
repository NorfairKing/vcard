{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Property.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import VCard.Property

instance GenValid FormattedName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Version where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
