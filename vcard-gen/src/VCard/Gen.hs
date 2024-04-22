{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Gen
  (
  )
where

import Data.GenValidity
import VCard
import VCard.Property.Gen ()

instance GenValid Card
