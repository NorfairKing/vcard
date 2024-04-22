{-# LANGUAGE TypeApplications #-}

module VCardSpec where

import Test.Syd
import Test.Syd.Validity
import VCard
import VCard.Gen ()

spec :: Spec
spec = do
  genValidSpec @VCard

  describe "vcardContentType" $
    it "is valid" $
      shouldBeValid vcardContentType
