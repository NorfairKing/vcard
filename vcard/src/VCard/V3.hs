{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.V3
  ( Card (..),
    mergeCards,
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
import VCard.Merge
import VCard.Property

data Card = Card
  { cardSources :: ![Source],
    cardFormattedNames :: !(NonEmpty FormattedName),
    cardName :: !Name,
    cardNicknames :: ![Nickname],
    cardEmails :: ![Email],
    cardTelephones :: ![Telephone],
    cardProductIdentifier :: !(Maybe ProductIdentifier),
    cardUID :: !(Maybe TextUID),
    cardRevision :: !(Maybe Revision),
    cardURLs :: ![URL]
  }
  deriving (Show, Eq, Generic)

instance Validity Card

instance NFData Card

instance IsComponent Card where
  componentName Proxy = "VCARD"
  componentP componentProperties = do
    cardVersion <- requiredPropertyP componentProperties
    when (cardVersion /= version3) $ unfixableError $ ComponentParseErrorVersionMismatch cardVersion version3

    cardSources <- listOfPropertiesP componentProperties
    cardFormattedNames <- nonemptyListOfPropertiesP componentProperties
    cardName <- unNameV3 <$> requiredPropertyP componentProperties
    cardNicknames <- listOfPropertiesP componentProperties
    cardEmails <- listOfPropertiesP componentProperties
    cardTelephones <- listOfPropertiesP componentProperties
    cardProductIdentifier <- optionalPropertyP componentProperties
    cardUID <- optionalPropertyP componentProperties
    cardRevision <- optionalPropertyP componentProperties
    cardURLs <- listOfPropertiesP componentProperties
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
        requiredPropertyB (Version "3.0"),
        listOfPropertiesB cardSources,
        nonemptyListOfPropertiesB cardFormattedNames,
        requiredPropertyB (NameV3 cardName),
        listOfPropertiesB cardNicknames,
        listOfPropertiesB cardEmails,
        listOfPropertiesB cardTelephones,
        optionalPropertyB cardProductIdentifier,
        optionalPropertyB cardUID,
        optionalPropertyB cardRevision,
        listOfPropertiesB cardURLs
      ]

-- Note: Only call this function if the two cards' UID matches.
--
-- TODO consider CLIENTPIDMAP
-- TODO consider PID
--
-- This prefers the second card's value in cases where a choice must be made, such as for the properties "N", "PRODID", and "UID"
mergeCards :: Card -> Card -> Card
mergeCards c1 c2 =
  Card
    { cardSources = mergeList cardSources c1 c2,
      cardFormattedNames = mergeNE cardFormattedNames c1 c2,
      cardName = mergeValue (cardName c1) (cardName c2),
      cardNicknames = mergeList cardNicknames c1 c2,
      cardEmails = mergeList cardEmails c1 c2,
      cardTelephones = mergeList cardTelephones c1 c2,
      cardProductIdentifier = mergeMaybe cardProductIdentifier c1 c2,
      cardUID = mergeMaybe cardUID c1 c2,
      cardRevision = mergeMaybe' max cardRevision c1 c2,
      cardURLs = mergeList cardURLs c1 c2
    }
