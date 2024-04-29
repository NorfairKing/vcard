{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Component.V4
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
    cardName :: !(Maybe Name),
    cardVersion :: !Version,
    cardNickname :: !(Maybe Nickname),
    cardGender :: !(Maybe Gender)
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

instance IsComponent Card where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    cardSource <- optionalPropertyP componentProperties
    cardFormattedName <- requiredPropertyP componentProperties
    cardName <- optionalPropertyP componentProperties
    cardVersion <- requiredPropertyP componentProperties
    cardNickname <- optionalPropertyP componentProperties
    cardGender <- optionalPropertyP componentProperties
    pure Card {..}
  componentB Card {..} =
    mconcat
      [ -- [Section 6.7.9: VERSION](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.9)
        --
        -- @
        -- Special notes:  This property MUST be present in the vCard object,
        --    and it must appear immediately after BEGIN:VCARD.  The value MUST
        --    be "4.0" if the vCard corresponds to this specification.  Note
        --    that earlier versions of vCard allowed this property to be placed
        --    anywhere in the vCard object, or even to be absent.
        -- @
        DList.singleton $ propertyContentLineB cardVersion,
        maybe mempty (DList.singleton . propertyContentLineB) cardSource,
        DList.singleton $ propertyContentLineB cardFormattedName,
        maybe mempty (DList.singleton . propertyContentLineB) cardName,
        maybe mempty (DList.singleton . propertyContentLineB) cardNickname,
        maybe mempty (DList.singleton . propertyContentLineB) cardGender
      ]
