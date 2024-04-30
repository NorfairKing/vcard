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

  describe "Source" $ do
    genValidSpec @Source
    propertySpec @Source

    -- [RFC 2425 Section 6.1](https://datatracker.ietf.org/doc/html/rfc2425#section-6.1)
    --
    -- @
    -- SOURCE;CONTEXT=LDAP:ldap://ldap.host/cn=Babs%20Jensen,%20o=Babsco,%20c=US
    -- @
    propertyParseExampleSpec
      "SOURCE;CONTEXT=LDAP:ldap://ldap.host/cn=Babs%20Jensen,%20o=Babsco,%20c=US"
      (mkSource "ldap://ldap.host/cn=Babs%20Jensen,%20o=Babsco,%20c=US")

    -- [RFC 2425 Section 8.2](https://datatracker.ietf.org/doc/html/rfc2425#section-8.2)
    --
    -- @
    -- source:ldap://cn=bjorn%20Jensen, o=university%20of%20Michigan, c=US
    -- @
    --
    -- [See erratum 7912](https://www.rfc-editor.org/errata/eid7912)
    propertyExampleSpec
      "source:ldap://cn=bjorn%20Jensen,o=university%20of%20Michigan,c=US"
      (mkSource "ldap://cn=bjorn%20Jensen,o=university%20of%20Michigan,c=US")

    -- [RFC 2425 Section 8.3](https://datatracker.ietf.org/doc/html/rfc2425#section-8.3)
    --
    -- @
    -- source:ldap://cn=Meister%20Berger,o=Universitaet%20Goerlitz,c=DE
    -- @
    propertyExampleSpec
      "source:ldap://cn=Meister%20Berger,o=Universitaet%20Goerlitz,c=DE"
      (mkSource "ldap://cn=Meister%20Berger,o=Universitaet%20Goerlitz,c=DE")

    -- [RFC 2425 Section 8.4](https://datatracker.ietf.org/doc/html/rfc2425#section-8.4)
    --
    -- @
    -- source:ldap://cn=Bjorn%20Jensen,o=University%20of%20Michigan,c=US
    -- @
    propertyExampleSpec
      "source:ldap://cn=Bjorn%20Jensen,o=University%20of%20Michigan,c=US"
      (mkSource "ldap://cn=Bjorn%20Jensen,o=University%20of%20Michigan,c=US")

    -- [RFC 6350 Section 6.1.3](https://datatracker.ietf.org/doc/html/rfc6350#section-6.1.3)
    --
    -- @
    -- Examples:
    --
    --   SOURCE:ldap://ldap.example.com/cn=Babs%20Jensen,%20o=Babsco,%20c=US
    --
    --   SOURCE:http://directory.example.com/addressbooks/jdoe/
    --    Jean%20Dupont.vcf
    -- @
    propertyExampleSpec
      "SOURCE:ldap://ldap.example.com/cn=Babs%20Jensen,%20o=Babsco,%20c=US"
      (mkSource "ldap://ldap.example.com/cn=Babs%20Jensen,%20o=Babsco,%20c=US")
    propertyExampleSpec
      "SOURCE:http://directory.example.com/addressbooks/jdoe/Jean%20Dupont.vcf"
      (mkSource "http://directory.example.com/addressbooks/jdoe/Jean%20Dupont.vcf")

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

    propertyExampleSpec "N:Foo,Bar;;;;" (mkName ["Foo", "Bar"] [] [] [] [])
    propertyExampleSpec "N:Foo\\;Bar;;;;" (mkName ["Foo;Bar"] [] [] [] [])
    propertyExampleSpec "N:Foo\\\\Bar;;;;" (mkName ["Foo\\Bar"] [] [] [] [])

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

  describe "Telephone" $ do
    genValidSpec @Telephone
    propertySpec @Telephone

    -- [RFC 2425 Section 8.2](https://datatracker.ietf.org/doc/html/rfc2425#section-8.2)
    --
    -- @
    -- tel;type=work,voice,msg:+1 313 747-4454
    -- @
    propertyParseExampleSpec "tel;type=work,voice,msg:+1 313 747-4454" (mkTelephoneText "+1 313 747-4454")
    propertyRenderExampleSpec "tel:+1 313 747-4454" (mkTelephoneText "+1 313 747-4454")

    -- [RFC 2425 Section 8.3](https://datatracker.ietf.org/doc/html/rfc2425#section-8.3)
    --
    -- @
    -- home.tel;type=fax,voice,msg:+49 3581 123456
    -- @
    propertyParseExampleSpec "home.tel;type=fax,voice,msg:+49 3581 123456" (mkTelephoneText "+49 3581 123456")
    propertyRenderExampleSpec "tel:+49 3581 123456" (mkTelephoneText "+49 3581 123456")

    -- [RFC 2426 Section 3.3.1](https://datatracker.ietf.org/doc/html/rfc2426#section-3.3.1)
    --
    -- @
    -- Type example:
    --
    --      TEL;TYPE=work,voice,pref,msg:+1-213-555-1234
    -- @
    propertyParseExampleSpec "TEL;TYPE=work,voice,pref,msg:+1-213-555-1234" (mkTelephoneText "+1-213-555-1234")
    propertyRenderExampleSpec "TEL:+1-213-555-1234" (mkTelephoneText "+1-213-555-1234")

    -- [RFC 2426 Section 7](https://datatracker.ietf.org/doc/html/rfc2426#section-7)
    --
    -- @
    -- TEL;TYPE=VOICE,MSG,WORK:+1-919-676-9515
    -- TEL;TYPE=FAX,WORK:+1-919-676-9564
    -- @
    propertyParseExampleSpec "TEL;TYPE=VOICE,MSG,WORK:+1-919-676-9515" (mkTelephoneText "+1-919-676-9515")
    propertyRenderExampleSpec "TEL:+1-919-676-9515" (mkTelephoneText "+1-919-676-9515")
    propertyParseExampleSpec "TEL;TYPE=FAX,WORK:+1-919-676-9564" (mkTelephoneText "+1-919-676-9564")
    propertyRenderExampleSpec "TEL:+1-919-676-9564" (mkTelephoneText "+1-919-676-9564")
    -- @
    -- TEL;TYPE=VOICE,MSG,WORK:+1-415-937-3419
    -- TEL;TYPE=FAX,WORK:+1-415-528-4164
    -- @
    propertyParseExampleSpec "TEL;TYPE=VOICE,MSG,WORK:+1-415-937-3419" (mkTelephoneText "+1-415-937-3419")
    propertyRenderExampleSpec "TEL:+1-415-937-3419" (mkTelephoneText "+1-415-937-3419")
    propertyParseExampleSpec "TEL;TYPE=FAX,WORK:+1-415-528-4164" (mkTelephoneText "+1-415-528-4164")
    propertyRenderExampleSpec "TEL:+1-415-528-4164" (mkTelephoneText "+1-415-528-4164")

    -- [RFC 2739 Section 2.3](https://datatracker.ietf.org/doc/html/rfc2739#section-2.3)
    --
    -- @
    -- TEL;WORK;MSG:+1-206-936-4544
    -- TEL;WORK;FAX:+1-206-936-7329
    -- @
    propertyParseExampleSpec "TEL;TYPE=WORK,MSG:+1-206-936-4544" (mkTelephoneText "+1-206-936-4544")
    propertyRenderExampleSpec "TEL:+1-206-936-4544" (mkTelephoneText "+1-206-936-4544")
    propertyParseExampleSpec "TEL;TYPE=WORK,FAX:+1-206-936-7329" (mkTelephoneText "+1-206-936-7329")
    propertyRenderExampleSpec "TEL:+1-206-936-7329" (mkTelephoneText "+1-206-936-7329")

    -- [RFC 2739 Section 6](https://datatracker.ietf.org/doc/html/rfc2739#section-6)
    --
    -- @
    -- TEL;TYPE=WORK,MSG:+1-206-937-9972
    -- TEL;TYPE=WORK,FAX:+1-206-936-7329
    -- @
    propertyParseExampleSpec "TEL;TYPE=WORK,MSG:+1-206-937-9972" (mkTelephoneText "+1-206-937-9972")
    propertyRenderExampleSpec "TEL:+1-206-937-9972" (mkTelephoneText "+1-206-937-9972")
    propertyParseExampleSpec "TEL;TYPE=WORK,FAX:+1-206-936-7329" (mkTelephoneText "+1-206-936-7329")
    propertyRenderExampleSpec "TEL:+1-206-936-7329" (mkTelephoneText "+1-206-936-7329")
    -- @
    -- TEL;TYPE=WORK,PREF:+1-617-693-8728
    -- TEL;TYPE=WORK,MSG:+1-919-676-9515
    -- @
    propertyParseExampleSpec "TEL;TYPE=WORK,PREF:+1-617-693-8728" (mkTelephoneText "+1-617-693-8728")
    propertyRenderExampleSpec "TEL:+1-617-693-8728" (mkTelephoneText "+1-617-693-8728")
    propertyParseExampleSpec "TEL;TYPE=WORK,MSG:+1-919-676-9515" (mkTelephoneText "+1-919-676-9515")
    propertyRenderExampleSpec "TEL:+1-919-676-9515" (mkTelephoneText "+1-919-676-9515")
    -- @
    -- TEL;TYPE=FAX:+1-617-693-8728
    -- TEL;TYPE=WORK,VOICE:423.875.2652
    -- TEL;TYPE=WORK,FAX:423.875.2017
    -- @
    propertyParseExampleSpec "TEL;TYPE=FAX:+1-617-693-8728" (mkTelephoneText "+1-617-693-8728")
    propertyRenderExampleSpec "TEL:+1-617-693-8728" (mkTelephoneText "+1-617-693-8728")
    propertyParseExampleSpec "TEL;TYPE=WORK,VOICE:423.875.2652" (mkTelephoneText "423.875.2652")
    propertyRenderExampleSpec "TEL:423.875.2652" (mkTelephoneText "423.875.2652")
    propertyParseExampleSpec "TEL;TYPE=WORK,FAX:423.875.2017" (mkTelephoneText "423.875.2017")
    propertyRenderExampleSpec "TEL:423.875.2017" (mkTelephoneText "423.875.2017")

    -- [RFC 6350 Section 6.4.1](https://datatracker.ietf.org/doc/html/rfc6350#section-6.4.1)
    --
    -- @
    -- Example:
    --
    --   TEL;VALUE=uri;PREF=1;TYPE="voice,home":tel:+1-555-555-5555;ext=5555
    --   TEL;VALUE=uri;TYPE=home:tel:+33-01-23-45-67
    -- @
    propertyParseExampleSpec "TEL;VALUE=uri;PREF=1;TYPE=\"voice,home\":tel:+1-555-555-5555;ext=5555" (mkTelephoneURI "tel:+1-555-555-5555;ext=5555")
    propertyRenderExampleSpec "TEL;VALUE=uri:tel:+1-555-555-5555;ext=5555" (mkTelephoneURI "tel:+1-555-555-5555;ext=5555")
    propertyParseExampleSpec "TEL;VALUE=uri;TYPE=home:tel:+33-01-23-45-67" (mkTelephoneURI "tel:+33-01-23-45-67")
    propertyRenderExampleSpec "TEL;VALUE=uri:tel:+33-01-23-45-67" (mkTelephoneURI "tel:+33-01-23-45-67")

    -- [RFC 6352 Section 6.3.2](https://datatracker.ietf.org/doc/html/rfc6352#section-6.3.2)
    --
    -- @
    -- TEL;TYPE=WORK,VOICE:412 605 0499
    -- TEL;TYPE=FAX:412 605 0705
    -- @
    propertyParseExampleSpec "TEL;TYPE=WORK,VOICE:412 605 0499" (mkTelephoneText "412 605 0499")
    propertyRenderExampleSpec "TEL:412 605 0499" (mkTelephoneText "412 605 0499")
    propertyParseExampleSpec "TEL;TYPE=FAX:412 605 0705" (mkTelephoneText "412 605 0705")
    propertyRenderExampleSpec "TEL:412 605 0705" (mkTelephoneText "412 605 0705")

  describe "Email" $ do
    genValidSpec @Email
    propertySpec @Email

    -- [RFC 2425 Section 8.1](https://datatracker.ietf.org/doc/html/rfc2425#section-8.1)
    --
    -- @
    -- email:babs@umich.edu
    -- @
    propertyExampleSpec "email:babs@umich.edu" (mkEmail "babs@umich.edu")

    -- [RFC2425 Section 8.4](https://datatracker.ietf.org/doc/html/rfc2425#section-8.4)
    --
    -- @
    -- email:bjorn@umich.edu
    -- @
    propertyExampleSpec "email:bjorn@umich.edu" (mkEmail "bjorn@umich.edu")

    -- [RFC2426 Section 3.3.2](https://datatracker.ietf.org/doc/html/rfc2426#section-3.3.2)
    --
    -- @
    -- Type example:
    --
    --      EMAIL;TYPE=internet:jqpublic@xyz.dom1.com
    --
    --      EMAIL;TYPE=internet:jdoe@isp.net
    --
    --      EMAIL;TYPE=internet,pref:jane_doe@abc.com
    -- @
    propertyParseExampleSpec "EMAIL;TYPE=internet:jqpublic@xyz.dom1.com" (mkEmail "jqpublic@xyz.dom1.com")
    propertyRenderExampleSpec "EMAIL:jqpublic@xyz.dom1.com" (mkEmail "jqpublic@xyz.dom1.com")
    propertyParseExampleSpec "EMAIL;TYPE=internet:jdoe@isp.net" (mkEmail "jdoe@isp.net")
    propertyRenderExampleSpec "EMAIL:jdoe@isp.net" (mkEmail "jdoe@isp.net")
    propertyParseExampleSpec "EMAIL;TYPE=internet,pref:jane_doe@abc.com" (mkEmail "jane_doe@abc.com")
    propertyRenderExampleSpec "EMAIL:jane_doe@abc.com" (mkEmail "jane_doe@abc.com")

    -- [RFC2426 Section 7](https://datatracker.ietf.org/doc/html/rfc2426#section-7)
    --
    -- @
    -- EMAIL;TYPE=INTERNET,PREF:Frank_Dawson@Lotus.com
    -- EMAIL;TYPE=INTERNET:fdawson@earthlink.net
    -- @
    propertyParseExampleSpec "EMAIL;TYPE=INTERNET,PREF:Frank_Dawson@Lotus.com" (mkEmail "Frank_Dawson@Lotus.com")
    propertyRenderExampleSpec "EMAIL:Frank_Dawson@Lotus.com" (mkEmail "Frank_Dawson@Lotus.com")
    propertyParseExampleSpec "EMAIL;TYPE=INTERNET:fdawson@earthlink.net" (mkEmail "fdawson@earthlink.net")
    propertyRenderExampleSpec "EMAIL:fdawson@earthlink.net" (mkEmail "fdawson@earthlink.net")
    -- @
    -- EMAIL;TYPE=INTERNET:howes@netscape.com
    -- @
    propertyParseExampleSpec "EMAIL;TYPE=INTERNET:howes@netscape.com" (mkEmail "howes@netscape.com")
    propertyRenderExampleSpec "EMAIL:howes@netscape.com" (mkEmail "howes@netscape.com")

    -- [RFC6350 Section 6.4.2](https://datatracker.ietf.org/doc/html/rfc6350#section-6.4.2)
    --
    -- @
    -- Example:
    --
    --         EMAIL;TYPE=work:jqpublic@xyz.example.com
    --
    --         EMAIL;PREF=1:jane_doe@example.com
    -- @
    propertyParseExampleSpec "EMAIL;TYPE=work:jqpublic@xyz.example.com" (mkEmail "jqpublic@xyz.example.com")
    propertyRenderExampleSpec "EMAIL:jqpublic@xyz.example.com" (mkEmail "jqpublic@xyz.example.com")
    propertyParseExampleSpec "EMAIL;PREF=1:jane_doe@example.com" (mkEmail "jane_doe@example.com")
    propertyRenderExampleSpec "EMAIL:jane_doe@example.com" (mkEmail "jane_doe@example.com")

  describe "UID" $ do
    describe "TextUID" $ do
      genValidSpec @TextUID
      propertySpec @TextUID

      -- [RFC 2426 Section 3.6.7](https://datatracker.ietf.org/doc/html/rfc6350#section-3.6.7)
      --
      -- @
      -- Type example:
      --
      --      UID:19950401-080045-40000F192713-0052
      -- @
      propertyExampleSpec "UID:19950401-080045-40000F192713-0052" (mkTextUID "19950401-080045-40000F192713-0052")

      -- [RFC 6352 Section 6.3.2](https://datatracker.ietf.org/doc/html/rfc6352#section-6.3.2)
      --
      -- @
      -- UID:1234-5678-9000-1
      -- @
      --
      propertyExampleSpec "UID:1234-5678-9000-1" (mkTextUID "1234-5678-9000-1")
      -- [RFC 6352 Section 8.6.3](https://datatracker.ietf.org/doc/html/rfc6352#section-8.6.3)
      -- and
      -- [RFC 6352 Section 8.6.4](https://datatracker.ietf.org/doc/html/rfc6352#section-8.6.4)
      --
      -- @
      -- UID:34222-232@example.com
      -- @
      propertyExampleSpec "UID:34222-232@example.com" (mkTextUID "34222-232@example.com")

    genValidSpec @UID
    propertySpec @UID

    -- [RFC 6350 Section 6.6.5](https://datatracker.ietf.org/doc/html/rfc6350#section-6.6.5)
    --
    -- @
    -- UID:urn:uuid:03a0e51f-d1aa-4385-8a53-e29025acd8af
    -- UID:urn:uuid:b8767877-b4a1-4c70-9acc-505d3819e519
    -- @
    propertyExampleSpec "UID:urn:uuid:03a0e51f-d1aa-4385-8a53-e29025acd8af" (mkUIDURI "urn:uuid:03a0e51f-d1aa-4385-8a53-e29025acd8af")
    propertyExampleSpec "UID:urn:uuid:b8767877-b4a1-4c70-9acc-505d3819e519" (mkUIDURI "urn:uuid:b8767877-b4a1-4c70-9acc-505d3819e519")

    -- [RFC 6350 Section 6.7.6](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.6)
    --
    -- @
    -- Example:
    --
    --         UID:urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6
    -- @
    propertyExampleSpec "UID:urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6" (mkUIDURI "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6")

    -- [RFC 6350 Section 7.2](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2)
    -- and
    -- [RFC 6350 Section 7.2.3](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.3)
    -- and
    -- [RFC 6350 Section 7.2.4](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.4)
    -- and
    -- [RFC 6350 Section 7.2.5](https://datatracker.ietf.org/doc/html/rfc6350#section-7.2.5)
    --
    -- @
    -- UID:urn:uuid:4fbe8971-0bc3-424c-9c26-36c3e1eff6b1
    -- @
    propertyExampleSpec "UID:urn:uuid:4fbe8971-0bc3-424c-9c26-36c3e1eff6b1" (mkUIDURI "urn:uuid:4fbe8971-0bc3-424c-9c26-36c3e1eff6b1")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:3.0" (Version "3.0")
    propertyExampleSpec "VERSION:4.0" (Version "4.0")
