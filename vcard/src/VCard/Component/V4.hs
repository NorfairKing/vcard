{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Component.V4
  ( Card (..),
    renderCard,
  )
where

import Control.DeepSeq
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Proxy
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import VCard.Component.Class
import VCard.ContentLine
import VCard.Property
import VCard.UnfoldedLine

data Card = Card
  { cardVersion :: !Version,
    cardFormattedName :: !FormattedName
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

instance IsComponent Card where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    cardVersion <- requiredPropertyP componentProperties
    cardFormattedName <- requiredPropertyP componentProperties
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
        DList.singleton $ propertyContentLineB cardFormattedName
      ]

renderCard :: Card -> Text
renderCard =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . namedComponentB
