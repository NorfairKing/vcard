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

  describe "Name" $ do
    genValidSpec @Name
    propertySpec @Name

    -- [RFC 2425 Section 8.2](https://datatracker.ietf.org/doc/html/rfc2425#section-8.2)
    --
    -- @
    -- n:Jensen;Bj=F8rn
    -- @
    propertyParseExampleLenientSpec "n:Jensen;Bj=F8rn" (mkName ["Jensen"] ["Bj=F8rn"] [] [] [])

    -- [RFC 2425 Section 8.3](https://datatracker.ietf.org/doc/html/rfc2425#section-8.3)
    --
    -- @
    -- n:Berger;Meister
    -- @
    propertyParseExampleLenientSpec "n:Berger;Meister" (mkName ["Berger"] ["Meister"] [] [] [])

    -- [RFC 2426 Section 3.6.5](https://datatracker.ietf.org/doc/html/rfc2426#section-3.6.5)
    --
    -- @
    -- N:van der Harten;Rene;J.;Sir;R.D.O.N.
    -- @
    propertyExampleSpec "N:van der Harten;Rene;J.;Sir;R.D.O.N." (mkName ["van der Harten"] ["Rene"] ["J."] ["Sir"] ["R.D.O.N."])
    -- @
    -- N:Pau;Shou Chang;Robert
    -- @
    propertyParseExampleLenientSpec "N:Pau;Shou Chang;Robert" (mkName ["Pau"] ["Shou Chang"] ["Robert"] [] [])
    -- @
    -- N:Koura;Osamu
    -- @
    propertyParseExampleLenientSpec "N:Koura;Osamu" (mkName ["Koura"] ["Osamu"] [] [] [])
    -- @
    -- N:del Pozo Triscon;Oscar
    -- @
    propertyParseExampleLenientSpec "N:del Pozo Triscon;Oscar" (mkName ["del Pozo Triscon"] ["Oscar"] [] [] [])
    -- @
    -- N:d'Aboville;Christine
    -- @
    propertyParseExampleLenientSpec "N:d'Aboville;Christine" (mkName ["d'Aboville"] ["Christine"] [] [] [])

    -- [RFC 6350 Section 5.4](https://datatracker.ietf.org/doc/html/rfc6350#section-5.4)
    --
    -- @
    -- N:Yamada;Taro;;;
    -- @
    propertyExampleSpec
      "N:Yamada;Taro;;;"
      ( mkName
          ["Yamada"]
          ["Taro"]
          []
          []
          []
      )

    -- [RFC 2739 Section 2.3](https://datatracker.ietf.org/doc/html/rfc2739#section-2.3)
    --
    -- @
    -- N:Dun;Alec
    -- @
    propertyParseExampleLenientSpec
      "N:Dun;Alec"
      (mkName ["Dun"] ["Alec"] [] [] [])

    -- [RFC 2739 Section 6](https://datatracker.ietf.org/doc/html/rfc2739#section-6)
    --
    -- @
    -- N:Small;Tony
    -- @
    propertyParseExampleLenientSpec "N:Small;Tony" (mkName ["Small"] ["Tony"] [] [] [])
    -- @
    -- N:Hennessy;Denis
    -- @
    propertyParseExampleLenientSpec "N:Hennessy;Denis" (mkName ["Hennessy"] ["Denis"] [] [] [])
    -- @
    -- N:Dawson;Frank
    -- @
    propertyParseExampleLenientSpec "N:Dawson;Frank" (mkName ["Dawson"] ["Frank"] [] [] [])
    -- @
    -- N:Egen;Pat
    -- @
    propertyParseExampleLenientSpec "N:Egen;Pat" (mkName ["Egen"] ["Pat"] [] [] [])

    -- [RFC 2426 Section 3.1.2](https://datatracker.ietf.org/doc/html/rfc2426#section-3.1.2)
    -- and
    -- [RFC 6350 Section 6.2.2](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.2)
    --
    -- @
    -- Examples:
    --
    --           N:Public;John;Quinlan;Mr.;Esq.
    --
    --           N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
    -- @
    propertyExampleSpec
      "N:Public;John;Quinlan;Mr.;Esq."
      ( mkName
          ["Public"]
          ["John"]
          ["Quinlan"]
          ["Mr."]
          ["Esq."]
      )
    propertyExampleSpec
      "N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P."
      ( mkName
          ["Stevenson"]
          ["John"]
          ["Philip", "Paul"]
          ["Dr."]
          ["Jr.", "M.D.", "A.C.P."]
      )
    -- [RFC 6350 Section 7.2.1](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.1)
    -- and
    -- [RFC 6350 Section 7.2.3](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.3)
    -- and
    -- [RFC 6350 Section 7.2.4](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.4)
    -- and
    -- [RFC 6350 Section 7.2.5](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.5)
    --
    -- @
    -- N:Doe;J.;;;
    -- @
    propertyExampleSpec
      "N:Doe;J.;;;"
      ( mkName
          ["Doe"]
          ["J."]
          []
          []
          []
      )
    -- [RFC 6350 Section 8](https://datatracker.ietf.org/doc/html/rfc6350#section-8)
    --
    -- @
    -- N:Perreault;Simon;;;ing. jr,M.Sc.
    -- @
    propertyExampleSpec
      "N:Perreault;Simon;;;ing. jr,M.Sc"
      (mkName ["Perreault"] ["Simon"] [] [] ["ing. jr", "M.Sc"])

    -- [RFC 6352 Section 6.3.2](https://datatracker.ietf.org/doc/html/rfc6352#section-6.3.2)
    propertyParseExampleLenientSpec
      "N:Daboo;Cyrus"
      (mkName ["Daboo"] ["Cyrus"] [] [] [])

  describe "Nickname" $ do
    genValidSpec @Nickname
    propertySpec @Nickname

    -- [RFC 2426 Section 3.1.3](https://datatracker.ietf.org/doc/html/rfc2426#section-3.1.3)
    -- and
    -- [RFC 6350 Section 6.2.3](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.3)
    --
    -- @
    -- Examples:
    --
    --           NICKNAME:Robbie
    --
    --           NICKNAME:Jim,Jimmie
    --
    --           NICKNAME;TYPE=work:Boss
    -- @
    propertyExampleSpec "NICKNAME:Robbie" (mkNickname ["Robbie"])
    propertyExampleSpec "NICKNAME:Jim,Jimmie" (mkNickname ["Jim", "Jimmie"])
    propertyExampleSpec "NICKNAME:Boss" (mkNickname ["Boss"])

    -- [RFC 6350 Section 6.2.3](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.3)
    -- @
    -- NICKNAME:me
    -- @
    propertyExampleSpec "NICKNAME:me" (mkNickname ["me"])

  describe "Gender" $ do
    genValidSpec @Sex
    genValidSpec @Gender
    propertySpec @Gender

    -- [RFC 6350 Section 6.2.7](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.7)
    --
    -- @
    -- Examples:
    --
    --   GENDER:M
    --   GENDER:F
    --   GENDER:M;Fellow
    --   GENDER:F;grrrl
    --   GENDER:O;intersex
    --   GENDER:;it's complicated
    -- @
    propertyExampleSpec "GENDER:M" (mkGender SexMale)
    propertyExampleSpec "GENDER:F" (mkGender SexFemale)
    propertyExampleSpec
      "GENDER:M;Fellow"
      ( Gender
          { genderSex = Just SexMale,
            genderIdentity = Just "Fellow"
          }
      )
    propertyExampleSpec
      "GENDER:F;grrrl"
      ( Gender
          { genderSex = Just SexFemale,
            genderIdentity = Just "grrrl"
          }
      )
    propertyExampleSpec
      "GENDER:O;intersex"
      ( Gender
          { genderSex = Just SexOther,
            genderIdentity = Just "intersex"
          }
      )
    propertyExampleSpec
      "GENDER:;it's complicated"
      ( Gender
          { genderSex = Nothing,
            genderIdentity = Just "it's complicated"
          }
      )

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:3.0" (Version "3.0")
    propertyExampleSpec "VERSION:4.0" (Version "4.0")
