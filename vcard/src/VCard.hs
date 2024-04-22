{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module VCard
  ( VCard (..),
    vcardContentType,
    module VCard,
  )
where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Validity
import GHC.Generics (Generic)

data VCard = VCard
  deriving (Show, Generic)

instance Validity VCard

instance NFData VCard

-- | MIME Content type
--
-- @
-- The text/vcard MIME content type (hereafter known as "vCard"; see
-- Section 10.1) contains contact information, typically pertaining to a
-- single contact or group of contacts.  The content consists of one or
-- more lines in the format given below.
-- @
--
-- @
-- The charset (see [RFC3536] for internationalization terminology) for
-- vCard is UTF-8 as defined in [RFC3629].  There is no way to override
-- this.  It is invalid to specify a value other than "UTF-8" in the
-- "charset" MIME parameter (see Section 10.1).
-- @
--
-- > vcardContentType = "text/calendar; charset=utf-8"
vcardContentType :: ByteString
vcardContentType = "text/vcard; charset=utf-8"
