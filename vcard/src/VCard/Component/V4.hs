{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.Component.V4
  ( Card (..),
  )
where

import Conformance
import Control.DeepSeq
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)
import VCard.Component.Class
import VCard.Property

data Card = Card
  { cardSources :: ![Source],
    cardFormattedNames :: !(NonEmpty FormattedName),
    cardName :: !(Maybe Name),
    cardNicknames :: ![Nickname],
    cardGender :: !(Maybe Gender),
    cardEmails :: ![Email]
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

instance IsComponent Card where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    cardVersion <- requiredPropertyP componentProperties
    when (cardVersion /= version4) $ unfixableError $ ComponentParseErrorVersionMismatch cardVersion version4

    cardSources <- listOfPropertiesP componentProperties
    cardFormattedNames <- nonemptyListOfPropertiesP componentProperties
    cardName <- optionalPropertyP componentProperties
    cardNicknames <- listOfPropertiesP componentProperties
    cardGender <- optionalPropertyP componentProperties
    cardEmails <- listOfPropertiesP componentProperties
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
        requiredPropertyB version4,
        listOfPropertiesB cardSources,
        nonemptyListOfPropertiesB cardFormattedNames,
        optionalPropertyB cardName,
        listOfPropertiesB cardNicknames,
        optionalPropertyB cardGender,
        listOfPropertiesB cardEmails
      ]
