{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Component.V3
  ( Card (..),
  )
where

import Control.DeepSeq
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)
import VCard.Component.Class
import VCard.Property

data Card = Card
  { cardSources :: ![Source],
    cardFormattedNames :: !(NonEmpty FormattedName),
    cardName :: !Name,
    cardVersion :: !Version,
    cardNicknames :: ![Nickname],
    cardEmails :: ![Email]
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

instance IsComponent Card where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    cardVersion <- requiredPropertyP componentProperties

    cardSources <- listOfPropertiesP componentProperties
    cardFormattedNames <- nonemptyListOfPropertiesP componentProperties
    cardName <- requiredPropertyP componentProperties
    cardNicknames <- listOfPropertiesP componentProperties
    cardEmails <- listOfPropertiesP componentProperties
    pure Card {..}
  componentB Card {..} =
    mconcat
      [ -- In version 4 and 3 (not 2.1), this must come immediately after BEGIN, see
        -- [RFC6350 Section 6.7.9: VERSION](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.9)
        --
        -- @
        -- Special notes:  This property MUST be present in the vCard object,
        --    and it must appear immediately after BEGIN:VCARD.  The value MUST
        --    be "4.0" if the vCard corresponds to this specification.  Note
        --    that earlier versions of vCard allowed this property to be placed
        --    anywhere in the vCard object, or even to be absent.
        -- @
        --
        -- For easy distinguishing between 2.1 and (3 and 4), we'll put it first here as well.
        DList.singleton $ propertyContentLineB cardVersion,
        DList.fromList $ map propertyContentLineB cardSources,
        DList.fromList $ map propertyContentLineB $ NE.toList cardFormattedNames,
        DList.singleton $ propertyContentLineB cardName,
        DList.fromList $ map propertyContentLineB cardNicknames,
        DList.fromList $ map propertyContentLineB cardEmails
      ]
