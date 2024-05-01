{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VCard.ParameterSpec where

import Test.Syd
import Test.Syd.Validity
import VCard.Parameter
import VCard.Parameter.Gen

spec :: Spec
spec = do
  describe "Language" $ do
    genValidSpec @Language
    parameterSpec @Language

    -- [RFC 6350 Section 5.1](https://datatracker.ietf.org/doc/html/rfc6350#section-5.1)
    -- @
    -- ROLE;LANGUAGE=tr:hoca
    -- @
    parameterExampleSpec "tr" (Language "tr")

    -- [RFC 6350 Section 5.4](https://datatracker.ietf.org/doc/html/rfc6350#section-5.4)
    --
    -- @
    --   N;ALTID=1;LANGUAGE=jp:<U+5C71><U+7530>;<U+592A><U+90CE>;;;
    --   N;ALTID=1;LANGUAGE=en:Yamada;Taro;;;
    --   (<U+XXXX> denotes a UTF8-encoded Unicode character.)
    --
    --   TITLE;ALTID=1;LANGUAGE=fr:Patron
    --   TITLE;ALTID=1;LANGUAGE=en:Boss
    --
    --   TITLE;ALTID=1;LANGUAGE=fr:Patron
    --   TITLE;ALTID=1;LANGUAGE=en:Boss
    --   TITLE;ALTID=2;LANGUAGE=en:Chief vCard Evangelist
    --
    -- while this one would not:
    --
    --   N;ALTID=1;LANGUAGE=jp:<U+5C71><U+7530>;<U+592A><U+90CE>;;;
    --   N:Yamada;Taro;;;
    --   (Two instances of the N property.)
    --
    -- and these three would be legal but questionable:
    --
    --   TITLE;ALTID=1;LANGUAGE=fr:Patron
    --   TITLE;ALTID=2;LANGUAGE=en:Boss
    --   (Should probably have the same ALTID value.)
    --
    --
    --
    --   TITLE;ALTID=1;LANGUAGE=fr:Patron
    --   TITLE:LANGUAGE=en:Boss
    --   (Second line should probably have ALTID=1.)
    --
    --   N;ALTID=1;LANGUAGE=jp:<U+5C71><U+7530>;<U+592A><U+90CE>;;;
    --   N;ALTID=1;LANGUAGE=en:Yamada;Taro;;;
    --   N;ALTID=1;LANGUAGE=en:Smith;John;;;
    --   (The last line should probably have ALTID=2.  But that would be
    --    illegal because N has cardinality *1.)
    -- @
    parameterExampleSpec "fr" (Language "fr")
    parameterExampleSpec "jp" (Language "jp")
    parameterExampleSpec "en" (Language "en")

    -- [RFC 2425 Section 8.3](https://datatracker.ietf.org/doc/html/rfc2425#section-8.3)
    --
    -- @
    -- title;language=de;value=text:Burgermeister
    -- @
    parameterExampleSpec "de" (Language "de")

  describe "Preference" $ do
    genValidSpec @Preference
    parameterSpec @Preference

    parameterExampleSpec "100" (Preference 100)

    -- [RFC 6350 Section 6.4.1](https://datatracker.ietf.org/doc/html/rfc2425#section-6.4.1)
    -- and
    -- [RFC 6350 Section 6.4.2](https://datatracker.ietf.org/doc/html/rfc2425#section-6.4.2)
    -- and
    -- [RFC 6350 Section 6.4.3](https://datatracker.ietf.org/doc/html/rfc2425#section-6.4.3)
    -- and
    -- [RFC 6350 Section 6.4.4](https://datatracker.ietf.org/doc/html/rfc2425#section-6.4.4)
    -- and
    -- [RFC 6350 Section 6.9.1](https://datatracker.ietf.org/doc/html/rfc2425#section-6.9.1)
    -- and
    -- [RFC 6350 Section 6.9.2](https://datatracker.ietf.org/doc/html/rfc2425#section-6.9.2)
    -- and
    -- [RFC 6350 Section 6.9.3](https://datatracker.ietf.org/doc/html/rfc2425#section-6.9.3)
    --
    -- @
    -- PREF=1
    -- @
    parameterExampleSpec "1" (Preference 1)

    -- [RFC 6350 Section 6.4.4](https://datatracker.ietf.org/doc/html/rfc2425#section-6.4.4)
    -- and
    -- [RFC 6350 Section 8](https://datatracker.ietf.org/doc/html/rfc2425#section-8)
    --
    -- @
    -- PREF=2
    -- @
    parameterExampleSpec "2" (Preference 2)
