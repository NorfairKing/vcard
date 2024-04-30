{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VCard.Property
  ( -- * Property
    PropertyParseError,
    PropertyParseFixableError,
    PropertyParseWarning,
    ConformProperty,
    IsProperty (..),

    -- ** Parsing/Rendering content lines
    propertyContentLineP,
    propertyContentLineB,

    -- ** Helpers for implementing IsProperty
    wrapPropertyTypeP,
    viaPropertyTypeP,
    viaPropertyTypeListP,

    -- ** Properties

    -- *** General properties

    -- **** Begin
    Begin (..),

    -- **** End
    End (..),

    -- **** Source
    Source (..),
    mkSource,

    -- *** Identification properties

    -- **** Formatted name
    FormattedName (..),
    mkFormattedName,

    -- **** Name
    Name (..),
    mkName,
    emptyName,

    -- **** Nickname
    Nickname (..),
    mkNickname,

    -- **** Gender
    Gender (..),
    mkGender,
    Sex (..),
    parseSex,
    renderSex,

    -- *** Communications properties
    Email (..),
    mkEmail,

    -- *** Explanatory properties

    -- **** Version
    Version (..),
    version3,
    version4,
  )
where

import Conformance
import Control.DeepSeq
import Control.Exception
import Data.CaseInsensitive (CI)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)
import VCard.ContentLine
import VCard.Parameter
import VCard.PropertyType

data PropertyParseError
  = PropertyTypeParseError !PropertyTypeParseError
  | MismatchedPropertyName
      -- Expected
      !ContentLineName
      -- Actual
      !ContentLineName
  deriving (Show, Eq, Generic)

instance Exception PropertyParseError where
  displayException = \case
    PropertyTypeParseError ptpe -> displayException ptpe
    MismatchedPropertyName expected actual ->
      unwords
        [ "Expected content line with name",
          show expected,
          "but got",
          show actual,
          "instead."
        ]

data PropertyParseFixableError
  = PropertyTypeParseFixableError !PropertyTypeParseFixableError
  | NameNotFiveComponents ![[Text]]
  | InvalidSex Text
  deriving (Show, Eq, Generic)

instance Exception PropertyParseFixableError where
  displayException = \case
    PropertyTypeParseFixableError ptpfe -> displayException ptpfe
    NameNotFiveComponents namess -> unwords ["Expected exactly five lists of names, but found:", show namess]
    InvalidSex sexText -> unwords ["Invalid sex:", show sexText]

type PropertyParseWarning = Void

type ConformProperty a = Conform PropertyParseError PropertyParseFixableError PropertyParseWarning a

-- | Calendar Properties
--
-- === [section 3.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7)
--
-- @
-- The Calendar Properties are attributes that apply to the iCalendar
-- object, as a whole.  These properties do not appear within a calendar
-- component.  They SHOULD be specified after the "BEGIN:VCALENDAR"
-- delimiter string and prior to any calendar component.
-- @
--
-- === Laws
--
-- * The 'ContentLineValue' that is built is valid:
--
-- >>> forAllValid $ \property -> isValid (propertyB property)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \contentLineValue -> isValid (propertyP contentlineValue)
--
-- * The property roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \property -> propertyP (propertyB property) == Right property
class IsProperty property where
  -- Name of the property
  propertyName :: Proxy property -> ContentLineName

  -- | Parser for the property
  propertyP :: ContentLineValue -> ConformProperty property

  -- | Builder for the property
  propertyB :: property -> ContentLineValue

propertyContentLineP ::
  forall property.
  (IsProperty property) =>
  ContentLine ->
  ConformProperty property
propertyContentLineP ContentLine {..} =
  let name = propertyName (Proxy :: Proxy property)
   in if contentLineName == name
        then propertyP contentLineValue
        else unfixableError $ MismatchedPropertyName name contentLineName

propertyContentLineB :: forall property. (IsProperty property) => property -> ContentLine
propertyContentLineB = ContentLine Nothing (propertyName (Proxy :: Proxy property)) . propertyB

wrapPropertyTypeP ::
  (IsPropertyType propertyType) =>
  (propertyType -> property) ->
  (ContentLineValue -> ConformProperty property)
wrapPropertyTypeP func = viaPropertyTypeP (pure . func)

viaPropertyTypeP ::
  forall propertyType property.
  (IsPropertyType propertyType) =>
  (propertyType -> ConformProperty property) ->
  (ContentLineValue -> ConformProperty property)
viaPropertyTypeP func clv = do
  propertyType <-
    conformMapAll PropertyTypeParseError PropertyTypeParseFixableError id $
      typedPropertyTypeP clv
  func propertyType

viaPropertyTypeListP ::
  forall propertyType property.
  (IsPropertyType propertyType) =>
  ([propertyType] -> ConformProperty property) ->
  (ContentLineValue -> ConformProperty property)
viaPropertyTypeListP func clv = do
  propertyType <-
    conformMapAll PropertyTypeParseError PropertyTypeParseFixableError id $
      propertyTypeListP clv
  func propertyType

propertyParamP ::
  (IsParameter param) =>
  ContentLineValue ->
  ConformProperty (Maybe param)
propertyParamP clv =
  conformMapAll (PropertyTypeParseError . ParameterParseError) (PropertyTypeParseFixableError . ParameterParseFixableError) id $
    optionalParam $
      contentLineValueParams clv

-- | BEGIN of a (VCARD) component
--
-- @
-- Purpose:  To denote the beginning of a syntactic entity within a
--    text/vcard content-type.
--
-- Value type:  text
--
-- Cardinality:  1
--
-- Special notes:  The content entity MUST begin with the BEGIN property
--    with a value of "VCARD".  The value is case-insensitive.
--
--    The BEGIN property is used in conjunction with the END property to
--    delimit an entity containing a related set of properties within a
--    text/vcard content-type.  This construct can be used instead of
--    including multiple vCards as body parts inside of a multipart/
--    alternative MIME message.  It is provided for applications that
--    wish to define content that can contain multiple entities within
--    the same text/vcard content-type or to define content that can be
--    identifiable outside of a MIME environment.
--
-- ABNF:
--
--   BEGIN-param = 0" "  ; no parameter allowed
--   BEGIN-value = "VCARD"
--
-- Example:
--
--       BEGIN:VCARD
-- @
newtype Begin = Begin {unBegin :: CI Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Begin

instance NFData Begin

instance IsProperty Begin where
  propertyName Proxy = "BEGIN"
  propertyP = wrapPropertyTypeP Begin
  propertyB = propertyTypeB . unBegin

-- | END of a (VCARD) component
newtype End = End {unEnd :: CI Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity End

instance NFData End

instance IsProperty End where
  propertyName Proxy = "END"
  propertyP = wrapPropertyTypeP End
  propertyB = propertyTypeB . unEnd

-- | Source
--
-- [RFC 6350 Section 6.1.3](https://datatracker.ietf.org/doc/html/rfc6350#section-6.1.3)
--
-- @
-- Purpose:  To identify the source of directory information contained
--    in the content type.
--
-- Value type:  uri
--
-- Cardinality:  *
--
-- Special notes:  The SOURCE property is used to provide the means by
--    which applications knowledgable in the given directory service
--    protocol can obtain additional or more up-to-date information from
--    the directory service.  It contains a URI as defined in [RFC3986]
--    and/or other information referencing the vCard to which the
--    information pertains.  When directory information is available
--    from more than one source, the sending entity can pick what it
--    considers to be the best source, or multiple SOURCE properties can
--    be included.
--
-- ABNF:
--
--   SOURCE-param = "VALUE=uri" / pid-param / pref-param / altid-param
--                / mediatype-param / any-param
--   SOURCE-value = URI
--
-- Examples:
--
--   SOURCE:ldap://ldap.example.com/cn=Babs%20Jensen,%20o=Babsco,%20c=US
--
--   SOURCE:http://directory.example.com/addressbooks/jdoe/
--    Jean%20Dupont.vcf
-- @
data Source = Source {sourceValue :: URI}
  deriving (Show, Eq, Ord, Generic)

instance Validity Source

instance NFData Source

instance IsProperty Source where
  propertyName Proxy = "SOURCE"
  propertyP = wrapPropertyTypeP Source
  propertyB = propertyTypeB . sourceValue

mkSource :: URI -> Source
mkSource sourceValue =
  Source {..}

-- [Section 6.2.1](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.1)
--
-- @
-- Purpose:  To specify the formatted text corresponding to the name of
--    the object the vCard represents.
--
-- Value type:  A single text value.
--
-- Cardinality:  1*
--
-- Special notes:  This property is based on the semantics of the X.520
--    Common Name attribute [CCITT.X520.1988].  The property MUST be
--    present in the vCard object.
--
-- ABNF:
--
--   FN-param = "VALUE=text" / type-param / language-param / altid-param
--            / pid-param / pref-param / any-param
--   FN-value = text
--
-- Example:
--
--       FN:Mr. John Q. Public\, Esq.
-- @
data FormattedName = FormattedName
  { formattedNameValue :: Text,
    formattedNameLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Generic)

instance Validity FormattedName

instance NFData FormattedName

instance IsProperty FormattedName where
  propertyName Proxy = "FN"
  propertyP clv = do
    formattedNameLanguage <- propertyParamP clv
    wrapPropertyTypeP (\formattedNameValue -> FormattedName {..}) clv
  propertyB FormattedName {..} =
    insertMParam formattedNameLanguage $
      propertyTypeB formattedNameValue

mkFormattedName :: Text -> FormattedName
mkFormattedName formattedNameValue =
  let formattedNameLanguage = Nothing
   in FormattedName {..}

-- [Section 6.2.2](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.2)
--
-- @
-- Purpose:  To specify the components of the name of the object the
--    vCard represents.
--
-- Value type:  A single structured text value.  Each component can have
--    multiple values.
--
-- Cardinality:  *1
--
-- Special note:  The structured property value corresponds, in
--    sequence, to the Family Names (also known as surnames), Given
--    Names, Additional Names, Honorific Prefixes, and Honorific
--    Suffixes.  The text components are separated by the SEMICOLON
--    character (U+003B).  Individual text components can include
--    multiple text values separated by the COMMA character (U+002C).
--    This property is based on the semantics of the X.520 individual
--    name attributes [CCITT.X520.1988].  The property SHOULD be present
--    in the vCard object when the name of the object the vCard
--    represents follows the X.520 model.
--
--    The SORT-AS parameter MAY be applied to this property.
--
-- ABNF:
--
--   N-param = "VALUE=text" / sort-as-param / language-param
--           / altid-param / any-param
--   N-value = list-component 4(";" list-component)
--
-- Examples:
--
--           N:Public;John;Quinlan;Mr.;Esq.
--
--           N:Stevenson;John;Philip,Paul;Dr.;Jr.,M.D.,A.C.P.
-- @
-- TODO in version 3, names didn't HAVE to have 5 component.
-- In that case we should not throw a fixable error at all.
data Name = Name
  { nameSurnames :: [Text],
    nameGivenNames :: [Text],
    nameAdditionalNames :: [Text],
    nameHonorificPrefixes :: [Text],
    nameHonorificSuffixes :: [Text],
    nameLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Generic)

instance Validity Name where
  validate n@Name {..} =
    let nonemptyNames f ns = decorate f $ decorateList ns $ \n' ->
          declare "The name is nonempty" $ not $ T.null n'
     in mconcat
          [ genericValidate n,
            nonemptyNames "nameSurnames" nameSurnames,
            nonemptyNames "nameGivenNames" nameGivenNames,
            nonemptyNames "nameAdditionalNames" nameAdditionalNames,
            nonemptyNames "nameHonorificPrefixes" nameHonorificPrefixes,
            nonemptyNames "nameHonorificSuffixes" nameHonorificSuffixes
          ]

instance NFData Name

instance IsProperty Name where
  propertyName Proxy = "N"
  propertyP clv = do
    nameLanguage <- propertyParamP clv
    let namess = map splitOnCommas (splitOnSemicolons (contentLineValueRaw clv))

    (nameSurnames, nameGivenNames, nameAdditionalNames, nameHonorificPrefixes, nameHonorificSuffixes) <- case namess of
      [n1, n2, n3, n4, n5] -> pure (n1, n2, n3, n4, n5)
      _ -> do
        emitFixableError $ NameNotFiveComponents namess
        pure $ case namess of
          (n1 : n2 : n3 : n4 : n5 : _) -> (n1, n2, n3, n4, n5)
          (n1 : n2 : n3 : n4 : _) -> (n1, n2, n3, n4, [])
          (n1 : n2 : n3 : _) -> (n1, n2, n3, [], [])
          (n1 : n2 : _) -> (n1, n2, [], [], [])
          (n1 : _) -> (n1, [], [], [], [])
          [] -> ([], [], [], [], [])
    pure Name {..}

  propertyB Name {..} =
    insertMParam nameLanguage $
      mkSimpleContentLineValue $
        T.intercalate
          ";"
          [ T.intercalate "," nameSurnames,
            T.intercalate "," nameGivenNames,
            T.intercalate "," nameAdditionalNames,
            T.intercalate "," nameHonorificPrefixes,
            T.intercalate "," nameHonorificSuffixes
          ]

mkName ::
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  Name
mkName nameSurnames nameGivenNames nameAdditionalNames nameHonorificPrefixes nameHonorificSuffixes =
  let nameLanguage = Nothing
   in Name {..}

emptyName :: Name
emptyName = mkName [] [] [] [] []

-- [Section 6.2.1](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.1)
--
-- @
-- Purpose:  To specify the text corresponding to the nickname of the
--    object the vCard represents.
--
-- Value type:  One or more text values separated by a COMMA character
--    (U+002C).
--
-- Cardinality:  *
--
-- Special note:  The nickname is the descriptive name given instead of
--    or in addition to the one belonging to the object the vCard
--    represents.  It can also be used to specify a familiar form of a
--    proper name specified by the FN or N properties.
--
-- ABNF:
--
--   NICKNAME-param = "VALUE=text" / type-param / language-param
--                  / altid-param / pid-param / pref-param / any-param
--   NICKNAME-value = text-list
--
-- Examples:
--
--           NICKNAME:Robbie
--
--           NICKNAME:Jim,Jimmie
--
--           NICKNAME;TYPE=work:Boss
-- @
data Nickname = Nickname
  { nicknameValues :: [Text],
    nicknameLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Generic)

instance Validity Nickname where
  validate nn@Nickname {..} =
    mconcat
      [ genericValidate nn,
        decorateList nicknameValues $ \n ->
          declare "The name is nonempty" $ not $ T.null n
      ]

instance NFData Nickname

instance IsProperty Nickname where
  propertyName Proxy = "NICKNAME"
  propertyP clv = do
    nicknameLanguage <- propertyParamP clv
    viaPropertyTypeListP
      ( \nicknameValues ->
          pure Nickname {..}
      )
      clv
  propertyB Nickname {..} =
    insertMParam nicknameLanguage $
      propertyTypeListB nicknameValues

mkNickname :: [Text] -> Nickname
mkNickname nicknameValues =
  let nicknameLanguage = Nothing
   in Nickname {..}

-- [RFC 6350 Section 6.2.7](https://datatracker.ietf.org/doc/html/rfc6350#section-6.2.7)
--
-- @
-- Purpose:  To specify the components of the sex and gender identity of
--    the object the vCard represents.
--
-- Value type:  A single structured value with two components.  Each
--    component has a single text value.
--
-- Cardinality:  *1
--
-- Special notes:  The components correspond, in sequence, to the sex
--    (biological), and gender identity.  Each component is optional.
--
--    Sex component:  A single letter.  M stands for "male", F stands
--       for "female", O stands for "other", N stands for "none or not
--       applicable", U stands for "unknown".
--
--    Gender identity component:  Free-form text.
--
-- ABNF:
--
--                 GENDER-param = "VALUE=text" / any-param
--                 GENDER-value = sex [";" text]
--
--                 sex = "" / "M" / "F" / "O" / "N" / "U"
--
-- Examples:
--
--   GENDER:M
--   GENDER:F
--   GENDER:M;Fellow
--   GENDER:F;grrrl
--   GENDER:O;intersex
--   GENDER:;it's complicated
-- @
data Gender = Gender
  { genderSex :: !(Maybe Sex),
    genderIdentity :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Validity Gender where
  validate g@Gender {..} =
    mconcat
      [ genericValidate g,
        case genderSex of
          Just _ -> valid
          Nothing -> case genderIdentity of
            Nothing -> valid
            Just identity ->
              declare "If there is no sex, but there is an identity, then the identity must not be empty" $
                not $
                  T.null identity
      ]

instance NFData Gender

instance IsProperty Gender where
  propertyName Proxy = "GENDER"
  propertyP clv = do
    let parts = splitOnSemicolons (contentLineValueRaw clv)
    case parts of
      [] -> do
        let genderSex = Nothing
        let genderIdentity = Nothing
        pure Gender {..}
      (sexText : rest) -> do
        genderSex <- case sexText of
          "" -> pure Nothing
          _ -> case parseSex sexText of
            Just sex -> pure $ Just sex
            Nothing -> do
              emitFixableError $ InvalidSex sexText
              pure Nothing
        let genderIdentity = case rest of
              [] -> Nothing
              (identityText : _) -> Just identityText -- TODO fixable error for extra parts
        pure Gender {..}
  propertyB Gender {..} =
    mkSimpleContentLineValue
      $ T.intercalate
        ";"
      $ concat
        [ [maybe "" renderSex genderSex],
          maybeToList genderIdentity
        ]

mkGender :: Sex -> Gender
mkGender s =
  Gender
    { genderSex = Just s,
      genderIdentity = Nothing
    }

data Sex
  = SexMale
  | SexFemale
  | SexOther
  | SexNone
  | SexUnknown
  deriving (Show, Eq, Generic)

instance Validity Sex

instance NFData Sex

parseSex :: Text -> Maybe Sex
parseSex = \case
  "M" -> Just SexMale
  "F" -> Just SexFemale
  "O" -> Just SexOther
  "N" -> Just SexNone
  "U" -> Just SexUnknown
  _ -> Nothing

renderSex :: Sex -> Text
renderSex = \case
  SexMale -> "M"
  SexFemale -> "F"
  SexOther -> "O"
  SexNone -> "N"
  SexUnknown -> "U"

-- | Email
--
-- [RFC6350 Section 6.4.2](https://datatracker.ietf.org/doc/html/rfc6350#section-6.4.2)
--
-- @
-- Purpose:  To specify the electronic mail address for communication
--    with the object the vCard represents.
--
-- Value type:  A single text value.
--
-- Cardinality:  *
--
-- Special notes:  The property can include tye "PREF" parameter to
--    indicate a preferred-use email address when more than one is
--    specified.
--
--    Even though the value is free-form UTF-8 text, it is likely to be
--    interpreted by a Mail User Agent (MUA) as an "addr-spec", as
--    defined in [RFC5322], Section 3.4.1.  Readers should also be aware
--    of the current work toward internationalized email addresses
--    [RFC5335bis].
--
-- ABNF:
--
--   EMAIL-param = "VALUE=text" / pid-param / pref-param / type-param
--               / altid-param / any-param
--   EMAIL-value = text
--
-- Example:
--
--         EMAIL;TYPE=work:jqpublic@xyz.example.com
--
--         EMAIL;PREF=1:jane_doe@example.com
-- @
data Email = Email
  { emailValue :: !Text
  }
  deriving (Show, Eq, Generic)

instance Validity Email

instance NFData Email

instance IsProperty Email where
  propertyName Proxy = "EMAIL"
  propertyP = wrapPropertyTypeP Email
  propertyB = propertyTypeB . emailValue

mkEmail :: Text -> Email
mkEmail emailValue =
  Email {..}

-- [RFC6350 Section 6.7.9](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.9)
--
-- @
-- Purpose:  To specify the version of the vCard specification used to
--    format this vCard.
--
-- Value type:  A single text value.
--
-- Cardinality:  1
--
-- Special notes:  This property MUST be present in the vCard object,
--    and it must appear immediately after BEGIN:VCARD.  The value MUST
--    be "4.0" if the vCard corresponds to this specification.  Note
--    that earlier versions of vCard allowed this property to be placed
--    anywhere in the vCard object, or even to be absent.
--
-- ABNF:
--
--   VERSION-param = "VALUE=text" / any-param
--   VERSION-value = "4.0"
--
-- Example:
--
--         VERSION:4.0
-- @
newtype Version = Version
  { unVersion :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Version

instance NFData Version

instance IsProperty Version where
  propertyName Proxy = "VERSION"
  propertyP = wrapPropertyTypeP Version
  propertyB = propertyTypeB . unVersion

version3 :: Version
version3 = Version "3.0"

version4 :: Version
version4 = Version "4.0"
