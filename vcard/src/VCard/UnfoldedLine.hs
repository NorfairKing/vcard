{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard.UnfoldedLine
  ( UnfoldedLine (..),
    UnfoldingError (..),
    UnfoldingFixableError (..),
    parseUnfoldedLines,
    renderUnfoldedLines,
  )
where

import Conformance
import Control.Exception
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Data.Validity
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)

-- | An unfolded line of text, the required newlines are already stripped.
--
-- @
-- Individual lines within vCard are delimited by the [RFC5322] line
-- break, which is a CRLF sequence (U+000D followed by U+000A).  Long
-- logical lines of text can be split into a multiple-physical-line
-- representation using the following folding technique.  Content lines
-- SHOULD be folded to a maximum width of 75 octets, excluding the line
-- break.  Multi-octet characters MUST remain contiguous.  The rationale
-- for this folding process can be found in [RFC5322], Section 2.1.1.
--
-- A logical line MAY be continued on the next physical line anywhere
-- between two characters by inserting a CRLF immediately followed by a
-- single white space character (space (U+0020) or horizontal tab
-- (U+0009)).  The folded line MUST contain at least one character.  Any
-- sequence of CRLF followed immediately by a single white space
-- character is ignored (removed) when processing the content type.  For
-- example, the line:
--
--   NOTE:This is a long description that exists on a long line.
--
-- can be represented as:
--
--   NOTE:This is a long description
--     that exists on a long line.
--
-- It could also be represented as:
--
--   NOTE:This is a long descrip
--    tion that exists o
--    n a long line.
--
-- The process of moving from this folded multiple-line representation
-- of a property definition to its single-line representation is called
-- unfolding.  Unfolding is accomplished by regarding CRLF immediately
-- followed by a white space character (namely, HTAB (U+0009) or SPACE
-- (U+0020)) as equivalent to no characters at all (i.e., the CRLF and
-- single white space character are removed).
--
--    Note: It is possible for very simple implementations to generate
--    improperly folded lines in the middle of a UTF-8 multi-octet
--    sequence.  For this reason, implementations SHOULD unfold lines in
--    such a way as to properly restore the original sequence.
--
--    Note: Unfolding is done differently than in [RFC5322].  Unfolding
--    in [RFC5322] only removes the CRLF, not the space following it.
--
-- Folding is done after any content encoding of a type value.
-- Unfolding is done before any decoding of a type value in a content
-- line.
-- @
newtype UnfoldedLine = UnfoldedLine {unUnfoldedLine :: Text}
  deriving stock (Eq, Generic)
  deriving newtype (Show, Read, IsString)

instance Validity UnfoldedLine -- TODO: requirement that it's a single line?

data UnfoldingError = NoCRLFAtEndError
  deriving (Show, Eq)

instance Exception UnfoldingError where
  displayException = \case
    NoCRLFAtEndError -> "Document did not end in a crlf."

data UnfoldingFixableError = NoCRLFAtEndFixableError
  deriving (Show, Eq)

instance Exception UnfoldingFixableError where
  displayException = \case
    NoCRLFAtEndFixableError -> "Document did not end in a crlf, but it ended in END:VCARD so tried to parse it anyway."

data UnfoldingWarning = LineTooLong
  deriving (Show, Eq)

instance Exception UnfoldingWarning where
  displayException = \case
    LineTooLong -> "Line was too long, so not folded according to spec: \"Content lines SHOULD be folded to a maximum width of 75 octets, excluding the line break.\""

-- TODO we can probably do something more efficient here with megaparsec.
parseUnfoldedLines :: Text -> Conform UnfoldingError UnfoldingFixableError Void [UnfoldedLine]
parseUnfoldedLines t
  | T.null t = pure []
  | otherwise = do
      let tryToParse =
            pure
              . map UnfoldedLine
              . init -- Ignore the last, empty, line
              -- [Section 3.2](https://datatracker.ietf.org/doc/html/rfc6350#section-3.2)
              -- @
              -- Individual lines within vCard are delimited by the [RFC5322] line
              -- break, which is a CRLF sequence (U+000D followed by U+000A).
              -- @
              . T.splitOn "\r\n"
              -- [Section 3.2](https://datatracker.ietf.org/doc/html/rfc6350#section-3.2)
              -- @
              -- Any
              -- sequence of CRLF followed immediately by a single white space
              -- character is ignored (removed) when processing the content type.
              -- @
              -- Replace a newline + tab character.
              . T.replace "\r\n\t" ""
              -- Replace a newline + space character.
              --
              -- [Section 3.2](https://datatracker.ietf.org/doc/html/rfc6350#section-3.2)
              -- @
              -- The process of moving from this folded multiple-line representation
              -- of a property definition to its single-line representation is called
              -- unfolding.  Unfolding is accomplished by regarding CRLF immediately
              -- followed by a white space character (namely, HTAB (U+0009) or SPACE
              -- (U+0020)) as equivalent to no characters at all (i.e., the CRLF and
              -- single white space character are removed).
              -- @
              . T.replace "\r\n " ""
      if T.takeEnd 2 t == "\r\n"
        then tryToParse t
        else do
          let t' = T.stripEnd t
          -- If the stream ends with END:VCARD then we don't have to be
          -- scared that it got cut off, so we can try to fix this error.
          if T.takeEnd (T.length "END:VCARD") t' == "END:VCARD"
            then do
              emitFixableError NoCRLFAtEndFixableError
              tryToParse (t' <> "\r\n")
            else unfixableError NoCRLFAtEndError

renderUnfoldedLines :: [UnfoldedLine] -> Text
renderUnfoldedLines = LT.toStrict . LTB.toLazyText . unfoldedLinesB

unfoldedLinesB :: [UnfoldedLine] -> Text.Builder
unfoldedLinesB = foldMap unfoldedLineB

unfoldedLineB :: UnfoldedLine -> Text.Builder
unfoldedLineB = go . unUnfoldedLine
  where
    -- [Section 3.2](https://datatracker.ietf.org/doc/html/rfc6350#section-3.2)
    -- @
    -- Content lines
    -- SHOULD be folded to a maximum width of 75 octets, excluding the line
    -- break.
    -- @
    go t =
      if T.length t < maxLineLen
        then LTB.fromText t <> LTB.fromString "\r\n"
        else LTB.fromText (T.take maxLineLen t) <> LTB.fromString "\r\n " <> go (T.drop maxLineLen t)
    maxLineLen = 73 -- Just to be sure
