{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Component.V3
  ( Card (..),
  )
where

import Control.DeepSeq
import qualified Data.DList as DList
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)
import VCard.Component.Class
import VCard.Property

data Card = Card
  { cardSource :: !(Maybe Source),
    cardFormattedName :: !FormattedName,
    cardName :: !Name,
    cardVersion :: !Version,
    cardNickname :: !(Maybe Nickname)
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

instance IsComponent Card where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    cardSource <- optionalPropertyP componentProperties
    cardFormattedName <- requiredPropertyP componentProperties
    cardName <- requiredPropertyP componentProperties
    cardVersion <- requiredPropertyP componentProperties
    cardNickname <- optionalPropertyP componentProperties
    pure Card {..}
  componentB Card {..} =
    mconcat
      [ -- In version 4 (not 3), this must come immediately after BEGIN, see
        -- [RFC2426 Section 6.7.9: VERSION](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.9)
        --
        -- @
        -- Special notes:  This property MUST be present in the vCard object,
        --    and it must appear immediately after BEGIN:VCARD.  The value MUST
        --    be "4.0" if the vCard corresponds to this specification.  Note
        --    that earlier versions of vCard allowed this property to be placed
        --    anywhere in the vCard object, or even to be absent.
        -- @
        --
        -- For easy distinguishing between 3 and 4, we'll put it first here as well.
        DList.singleton $ propertyContentLineB cardVersion,
        maybe mempty (DList.singleton . propertyContentLineB) cardSource,
        DList.singleton $ propertyContentLineB cardFormattedName,
        DList.singleton $ propertyContentLineB cardName,
        maybe mempty (DList.singleton . propertyContentLineB) cardNickname
      ]
