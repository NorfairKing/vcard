{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Gen
  (
  )
where

import Data.GenValidity
import VCard

instance GenValid VCard
