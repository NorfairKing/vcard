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

    -- [RFC 6350 Section 5.9](https://datatracker.ietf.org/doc/html/rfc6350#section-5.9)
    --
    -- @
    -- FN:Rene van der Harten
    -- N;SORT-AS="Harten,Rene":van der Harten;Rene,J.;Sir;R.D.O.N.
    --
    -- FN:Robert Pau Shou Chang
    -- N;SORT-AS="Pau Shou Chang,Robert":Shou Chang;Robert,Pau;;
    --
    -- FN:Osamu Koura
    -- N;SORT-AS="Koura,Osamu":Koura;Osamu;;
    --
    -- FN:Oscar del Pozo
    -- N;SORT-AS="Pozo,Oscar":del Pozo Triscon;Oscar;;
    --
    -- FN:Chistine d'Aboville
    -- N;SORT-AS="Aboville,Christine":d'Aboville;Christine;;
    --
    -- FN:H. James de Mann
    -- N;SORT-AS="Mann,James":de Mann;Henry,James;;
    -- @
    propertyExampleSpec "FN:John Doe" (mkFormattedName "John Doe")
    propertyExampleSpec "FN:Rene van der Harten" (mkFormattedName "Rene van der Harten")
    propertyExampleSpec "FN:Robert Pau Shou Chang" (mkFormattedName "Robert Pau Shou Chang")
    propertyExampleSpec "FN:Osamu Koura" (mkFormattedName "Osamu Koura")
    propertyExampleSpec "FN:Oscar del Pozo" (mkFormattedName "Oscar del Pozo")
    propertyExampleSpec "FN:Chistine d'Aboville" (mkFormattedName "Chistine d'Aboville")
    propertyExampleSpec "FN:H. James de Mann" (mkFormattedName "H. James de Mann")

    -- [RFC 6350 Section 6.1.4](https://datatracker.ietf.org/doc/html/rfc6350#section-6.1.4)
    --
    -- @
    --       FN:Jane Doe
    -- @
    propertyExampleSpec "FN:Jane Doe" (mkFormattedName "Jane Doe")
    -- @
    --       FN:ABC Marketing
    -- @
    propertyExampleSpec "FN:ABC Marketing" (mkFormattedName "ABC Marketing")

    -- [RFC 6350 Section 6.2.1](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.1)
    --
    -- @
    -- Example:
    --
    --       FN:Mr. John Q. Public\, Esq.
    -- @
    propertyExampleSpec "FN:Mr. John Q. Public\\, Esq." (mkFormattedName "Mr. John Q. Public, Esq.")

    -- [RFC 6350 Section 6.6.5](https://datatracker.ietf.org/doc/html/rfc6350#section-6.6.5)
    --
    -- @
    -- FN:The Doe family
    -- @
    propertyExampleSpec "FN:The Doe family" (mkFormattedName "The Doe family")
    -- @
    -- FN:John Doe
    -- @
    propertyExampleSpec "FN:John Doe" (mkFormattedName "John Doe")
    -- @
    -- FN:Jane Doe
    -- @
    propertyExampleSpec "FN:Jane Doe" (mkFormattedName "Jane Doe")
    -- @
    -- FN:Funky distribution list
    -- @
    propertyExampleSpec "FN:Funky distribution list" (mkFormattedName "Funky distribution list")

    -- [RFC 6350 Section 7.2.4](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.4)
    -- and
    -- [RFC 6350 Section 7.2.5](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.5)
    --
    -- @
    -- FN:J. Doe
    -- @
    propertyExampleSpec "FN:J. Doe" (mkFormattedName "J. Doe")

    -- [RFC 6350 Section 8](https://datatracker.ietf.org/doc/html/rfc6350#section-8)
    -- and
    -- [RFC 6350 Section 8](https://datatracker.ietf.org/doc/html/rfc6350#section-8)
    --
    -- @
    -- FN:Simon Perreault
    -- @
    propertyExampleSpec "FN:Simon Perreault" (mkFormattedName "Simon Perreault")

    -- [RFC 2425 Section 8.2](https://datatracker.ietf.org/doc/html/rfc2425#section-8.2)
    --
    -- @
    -- fn:Bj=F8rn Jensen
    -- @
    propertyExampleSpec "FN:Bj=F8rn Jensen" (mkFormattedName "Bj=F8rn Jensen")

    -- [RFC 2425 Section 8.3](https://datatracker.ietf.org/doc/html/rfc2425#section-8.3)
    --
    -- @
    -- fn:Meister Berger
    -- @
    propertyExampleSpec "FN:Meister Berger" (mkFormattedName "Meister Berger")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:3.0" (Version "3.0")
    propertyExampleSpec "VERSION:4.0" (Version "4.0")
