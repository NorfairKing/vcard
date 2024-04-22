{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.ContentLine
  ( ContentLine (..),
    mkSimpleContentLine,
    ContentLineGroup (..),
    ContentLineName (..),
    ContentLineValue (..),
    emptyContentLineValue,
    mkSimpleContentLineValue,
    parseContentLineFromUnfoldedLine,
    renderContentLineToUnfoldedLine,
    ParamName (..),
    ParamValue (..),
    paramValueText,
    textToParamValue,
    haveToQuoteText,
    escapeParamValue,
    unescapeParamValue,

    -- * Raw parser
    P,
    contentLineP,
    contentLineNameP,
    contentLineValueP,
    paramNameP,
    paramValueP,

    -- * Raw builders
    contentLineB,
    contentLineGroupB,
    renderContentLineGroup,
    contentLineNameB,
    renderContentLineName,
    contentLineValueB,
    paramNameB,
    paramValueB,

    -- * Validation helpers
    validateSafeChar,
    validateQSafeChar,
    validateGroupChar,
    validateNameChar,
    validateVendorIdChar,
  )
where

import Control.Arrow (left)
import Control.DeepSeq
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char as Char
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Data.Validity
import Data.Validity.CaseInsensitive ()
import Data.Validity.Containers ()
import Data.Validity.Text
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import VCard.UnfoldedLine

-- | Content Line
--
-- [section 3.3](https://datatracker.ietf.org/doc/html/rfc6350#section-3.3)
data ContentLine = ContentLine
  { contentLineGroup :: !(Maybe ContentLineGroup),
    contentLineName :: !ContentLineName,
    contentLineValue :: !ContentLineValue
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLine

instance NFData ContentLine

instance IsString ContentLine where
  fromString s =
    let t = fromString s
     in case parse contentLineP "" t of
          Left err -> error $ errorBundlePretty err
          Right cln -> cln

mkSimpleContentLine :: CI Text -> Text -> ContentLine
mkSimpleContentLine name value =
  ContentLine
    { contentLineGroup = Nothing,
      contentLineName = ContentLineName name,
      contentLineValue = mkSimpleContentLineValue value
    }

-- [section 3.3](https://datatracker.ietf.org/doc/html/rfc6350#section-3.3)
-- @
-- Group and name are case-insensitive.
-- @
newtype ContentLineGroup = ContentLineGroup {unContentLineGroup :: CI Text}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLineGroup where
  validate cln@(ContentLineGroup t) =
    mconcat
      [ genericValidate cln,
        declare "The name is not empty" $ not $ T.null $ CI.original t,
        decorateText (CI.original t) validateGroupChar
      ]

instance NFData ContentLineGroup

-- [section 3.3](https://datatracker.ietf.org/doc/html/rfc6350#section-3.3)
-- @
-- Group and name are case-insensitive.
-- @
newtype ContentLineName = ContentLineName {unContentLineName :: CI Text}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLineName where
  validate cln@(ContentLineName t) =
    mconcat
      [ genericValidate cln,
        declare "The name is not empty" $ not $ T.null $ CI.original t,
        decorateText (CI.original t) validateNameChar
      ]

instance NFData ContentLineName

instance IsString ContentLineName where
  fromString s =
    let t = fromString s
     in case parse contentLineNameP "" t of
          Left err -> error $ errorBundlePretty err
          Right cln -> cln

data ContentLineValue = ContentLineValue
  { contentLineValueParams :: !(Map ParamName (NonEmpty ParamValue)),
    contentLineValueRaw :: !Text
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLineValue

instance NFData ContentLineValue

mkSimpleContentLineValue :: Text -> ContentLineValue
mkSimpleContentLineValue value =
  ContentLineValue
    { contentLineValueParams = M.empty,
      contentLineValueRaw = value
    }

emptyContentLineValue :: ContentLineValue
emptyContentLineValue = mkSimpleContentLineValue ""

-- | Parameter Name
--
-- [Section 3.3](https://datatracker.ietf.org/doc/html/rfc6350#section-3.3)
--
-- @
-- Property names and parameter names are case-insensitive (e.g., the
-- property name "fn" is the same as "FN" and "Fn").
-- @
newtype ParamName = ParamName {unParamName :: CI Text}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ParamName where
  validate pn@(ParamName t) =
    mconcat
      [ genericValidate pn,
        declare "The name is not empty" $ not $ T.null $ CI.original t,
        decorateText (CI.original t) validateNameChar
      ]

instance NFData ParamName

instance IsString ParamName where
  fromString s =
    let t = fromString s
     in case parse paramNameP "" t of
          Left err -> error $ errorBundlePretty err
          Right pn -> pn

-- | Parameter Value
--
-- [Section 3.3](https://datatracker.ietf.org/doc/html/rfc6350#section-3.3)
--
-- @
-- Parameter values
-- MAY be case-sensitive or case-insensitive, depending on their
-- definition.  Parameter values that are not explicitly defined as
-- being case-sensitive are case-insensitive.  Based on experience with
-- vCard 3 interoperability, it is RECOMMENDED that property and
-- parameter names be upper-case on output.
-- @
data ParamValue
  = UnquotedParam !Text
  | QuotedParam !Text
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ParamValue

instance NFData ParamValue

instance IsString ParamValue where
  fromString = textToParamValue . fromString

paramValueText :: ParamValue -> Text
paramValueText = \case
  UnquotedParam t -> t
  QuotedParam t -> t

textToParamValue :: Text -> ParamValue
textToParamValue t =
  if haveToQuoteText t
    then QuotedParam t
    else UnquotedParam t

-- @
-- When parsing a content line, folded lines MUST first be unfolded
-- according to the unfolding procedure described above.
-- @
parseContentLineFromUnfoldedLine :: UnfoldedLine -> Either String ContentLine
parseContentLineFromUnfoldedLine (UnfoldedLine t) =
  left errorBundlePretty $
    parse contentLineP "" t

renderContentLineToUnfoldedLine :: ContentLine -> UnfoldedLine
renderContentLineToUnfoldedLine =
  UnfoldedLine . LT.toStrict . LTB.toLazyText . contentLineB

type P = Parsec Void Text

-- contentline = [group "."] name *(";" param) ":" value CRLF
contentLineP :: P ContentLine
contentLineP = do
  contentLineGroup <- optional $ try $ do
    g <- contentLineGroupP
    void $ char '.'
    pure g
  contentLineName <- contentLineNameP
  contentLineValue <- contentLineValueP
  pure ContentLine {..}

-- group = 1*(ALPHA / DIGIT / "-")
contentLineGroupP :: P ContentLineGroup
contentLineGroupP = ContentLineGroup <$> tokenTextP

-- name          = iana-token / x-name
--
-- iana-token    = 1*(ALPHA / DIGIT / "-")
-- ; iCalendar identifier registered with IANA
-- x-name        = "X-" [vendorid "-"] 1*(ALPHA / DIGIT / "-")
-- ; Reserved for experimental use.
-- vendorid      = 3*(ALPHA / DIGIT)
-- ; Vendor identification
contentLineNameP :: P ContentLineName
contentLineNameP = ContentLineName <$> tokenTextP

-- contentline = name * (";" param) ":" value CRLF
contentLineValueP :: P ContentLineValue
contentLineValueP = do
  contentLineValueParams <- fmap M.fromList $
    many $ do
      void $ char ';'
      paramP
  void $ char ':'
  contentLineValueRaw <- contentLineValueRawP
  pure ContentLineValue {..}

-- value         = *VALUE-CHAR
--
-- VALUE-CHAR    = WSP / %x21-7E / NON-US-ASCII
-- ; Any textual character
contentLineValueRawP :: P Text
contentLineValueRawP = takeRest

-- iana-token    = 1*(ALPHA / DIGIT / "-")
-- ; iCalendar identifier registered with IANA
tokenTextP :: P (CI Text)
tokenTextP = CI.mk . T.pack <$> some (letterChar <|> digitChar <|> char '-')

validateGroupChar :: Char -> Validation
validateGroupChar c =
  declare "The character is a name character" $
    Char.isAlpha c || Char.isDigit c || c == '-'

validateNameChar :: Char -> Validation
validateNameChar c =
  declare "The character is a name character" $
    Char.isAlpha c || Char.isDigit c || c == '-'

validateVendorIdChar :: Char -> Validation
validateVendorIdChar c =
  declare "The character is a letter or a digit" $
    Char.isAlpha c || Char.isDigit c

-- param         = param-name "=" param-value *("," param-value)
-- ; Each property defines the specific ABNF for the parameters
-- ; allowed on the property.  Refer to specific properties for
-- ; precise parameter ABNF.
paramP :: P (ParamName, NonEmpty ParamValue)
paramP = do
  name <- paramNameP
  void $ char' '='
  firstValue <- paramValueP
  restOfValues <- many $ do
    void $ char' ','
    paramValueP
  pure (name, firstValue :| restOfValues)

-- param-name    = iana-token / x-name
paramNameP :: P ParamName
paramNameP = ParamName <$> tokenTextP

-- param-value   = paramtext / quoted-string
paramValueP :: P ParamValue
paramValueP =
  try (QuotedParam <$> quotedStringP)
    <|> (UnquotedParam <$> paramTextP)

--  paramtext     = *SAFE-CHAR
paramTextP :: P Text
paramTextP = unescapeParamValue . T.pack <$> many safeCharP

-- quoted-string = DQUOTE *QSAFE-CHAR DQUOTE
quotedStringP :: P Text
quotedStringP = do
  void $ char' '"'
  t <- unescapeParamValue . T.pack <$> many qSafeCharP
  void $ char' '"'
  pure t

-- QSAFE-CHAR    = WSP / %x21 / %x23-7E / NON-US-ASCII
-- ; Any character except CONTROL and DQUOTE
qSafeCharP :: P Char
qSafeCharP = satisfy $ validationIsValid . validateQSafeChar

validateQSafeChar :: Char -> Validation
validateQSafeChar c = do
  let o = ord c
  declare "The character is a quote-safe character" $ case c of
    _ | o < 0x09 -> True
    '\t' -> True -- 0x09, part of WSP
    _ | 0x09 < o && o < 0x20 -> True
    ' ' -> True -- 0x20, part of WSP
    _ | 0x21 == o -> True -- %x21
    '"' -> False -- 0x22
    _ | 0x23 <= o && o <= 0x7E -> True -- %x23-7E
    '\DEL' -> False -- 0x7F
    _ | o >= 0x80 -> True -- NON-US-ASCII
    _ -> False

-- SAFE-CHAR     = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E
--               / NON-US-ASCII
-- ; Any character except CONTROL, DQUOTE, ";", ":", ","
safeCharP :: P Char
safeCharP = satisfy $ validationIsValid . validateSafeChar

validateSafeChar :: Char -> Validation
validateSafeChar c = do
  let o = ord c
  declare "The character is a safe character" $ case c of
    _ | o < 0x09 -> True
    '\t' -> True -- 0x09, part of WSP
    _ | 0x09 < o && o < 0x20 -> True
    ' ' -> True -- 0x20, part of WSP
    _ | 0x21 == o -> True -- %x21
    '"' -> False -- 0x22
    _ | 0x23 <= o && o <= 0x2B -> True -- %x23-2B
    ',' -> False -- 0x2C
    _ | 0x2D <= o && o <= 0x39 -> True -- %x2D-39
    ':' -> False -- 0x3A
    ';' -> False -- 0x3B
    _ | 0x3C <= o && o <= 0x7E -> True -- %x3C-7E
    '\DEL' -> False -- 0x7F
    _ | o >= 0x80 -> True -- NON-US-ASCII
    _ -> False

contentLineB :: ContentLine -> Text.Builder
contentLineB ContentLine {..} =
  mconcat
    [ case contentLineGroup of
        Nothing -> mempty
        Just g -> contentLineGroupB g <> ".",
      contentLineNameB contentLineName,
      contentLineValueB contentLineValue
    ]

contentLineValueB :: ContentLineValue -> Text.Builder
contentLineValueB ContentLineValue {..} =
  mconcat
    [ contentLineParamsB contentLineValueParams,
      LTB.singleton ':',
      LTB.fromText contentLineValueRaw
    ]

renderContentLineGroup :: ContentLineGroup -> Text
renderContentLineGroup = LT.toStrict . LTB.toLazyText . contentLineGroupB

contentLineGroupB :: ContentLineGroup -> Text.Builder
contentLineGroupB = LTB.fromText . CI.original . unContentLineGroup

renderContentLineName :: ContentLineName -> Text
renderContentLineName = LT.toStrict . LTB.toLazyText . contentLineNameB

contentLineNameB :: ContentLineName -> Text.Builder
contentLineNameB = LTB.fromText . CI.original . unContentLineName

contentLineParamsB :: Map ParamName (NonEmpty ParamValue) -> Text.Builder
contentLineParamsB = foldMap go . M.toList
  where
    go :: (ParamName, NonEmpty ParamValue) -> Text.Builder
    go (key, values) =
      mconcat
        [ LTB.singleton ';',
          paramNameB key,
          LTB.singleton '=',
          paramValuesB values
        ]

paramValuesB :: NonEmpty ParamValue -> Text.Builder
paramValuesB = mconcat . intersperse (LTB.singleton ',') . map paramValueB . NE.toList

-- From [Section 3](https://datatracker.ietf.org/doc/html/rfc6868#section-3)
-- @
-- Based on experience with
-- vCard 3 interoperability, it is RECOMMENDED that property and
-- parameter names be upper-case on output.
-- @
paramNameB :: ParamName -> Text.Builder
paramNameB = LTB.fromText . T.toUpper . CI.original . unParamName

paramValueB :: ParamValue -> Text.Builder
paramValueB = \case
  -- From [RFC 6868 Section 3](https://datatracker.ietf.org/doc/html/rfc6868#section-3)
  --
  -- @
  -- The
  -- \^-escaping mechanism can be used when the value is either unquoted or
  -- quoted (i.e., whether or not the value is surrounded by double-
  -- quotes).
  -- @
  UnquotedParam t -> LTB.fromText (escapeParamValue t)
  QuotedParam t ->
    mconcat
      [ LTB.singleton '"',
        LTB.fromText (escapeParamValue t),
        LTB.singleton '"'
      ]

-- | Escape a parameter value
--
-- From [RFC 6868 Section 3](https://datatracker.ietf.org/doc/html/rfc6868#section-3)
--
-- @
-- When generating iCalendar or vCard parameter values, the following
-- apply:
--
-- o  formatted text line breaks are encoded into ^n (U+005E, U+006E)
--
-- o  the ^ character (U+005E) is encoded into ^^ (U+005E, U+005E)
--
-- o  the " character (U+0022) is encoded into ^' (U+005E, U+0027)
-- @
escapeParamValue :: Text -> Text
escapeParamValue = T.concatMap go
  where
    -- FIXME this could probably go a LOT faster but we need to benchmark it
    go = \case
      '\n' -> "^n"
      '^' -> "^^"
      '"' -> "^'"
      c -> T.singleton c

-- | Un-escape a parameter value
--
-- From [RFC 6868 Section 3](https://datatracker.ietf.org/doc/html/rfc6868#section-3)
--
-- @
-- When parsing iCalendar or vCard parameter values, the following
-- apply:
--
-- o  the character sequence ^n (U+005E, U+006E) is decoded into an
--    appropriate formatted line break according to the type of system
--    being used
--
-- o  the character sequence ^^ (U+005E, U+005E) is decoded into the ^
--    character (U+005E)
--
-- o  the character sequence ^' (U+005E, U+0027) is decoded into the "
--    character (U+0022)
--
-- o  if a ^ (U+005E) character is followed by any character other than
--    the ones above, parsers MUST leave both the ^ and the following
--    character in place
-- @
unescapeParamValue :: Text -> Text
unescapeParamValue = T.pack . go . T.unpack
  where
    -- FIXME this could probably go a LOT faster

    go = \case
      [] -> []
      '^' : 'n' : rest -> '\n' : go rest
      '^' : '^' : rest -> '^' : go rest
      '^' : '\'' : rest -> '"' : go rest
      c : rest -> c : go rest

haveToQuoteText :: Text -> Bool
haveToQuoteText = T.any haveToQuoteChar

haveToQuoteChar :: Char -> Bool
haveToQuoteChar = \case
  ';' -> True
  ':' -> True
  ',' -> True
  c -> Char.isControl c
