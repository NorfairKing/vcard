{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VCard.PropertySpec where

import Test.Syd
import Test.Syd.Validity hiding (Location)
import VCard.Property
import VCard.Property.Gen

spec :: Spec
spec = do
  describe "Begin" $ do
    genValidSpec @Begin
    propertySpec @Begin
    propertyExampleSpec "BEGIN:VCARD" (Begin "VCARD")

  describe "End" $ do
    genValidSpec @End
    propertySpec @End
    propertyExampleSpec "END:VCARD" (End "VCARD")

  describe "FormattedName" $ do
    genValidSpec @FormattedName
    propertySpec @FormattedName
    propertyExampleSpec "FN:John Doe" (FormattedName "John Doe")
    -- @
    -- Example:
    --
    --       FN:Mr. John Q. Public\, Esq.
    -- @
    propertyExampleSpec "FN:Mr. John Q. Public\\, Esq." (FormattedName "Mr. John Q. Public, Esq.")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:3.0" (Version "3.0")
    propertyExampleSpec "VERSION:4.0" (Version "4.0")
