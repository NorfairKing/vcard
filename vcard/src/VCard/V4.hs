{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VCard.V4
  ( Card (..),
    fromV3,
    toV3,
    mergeCards,
  )
where

import Conformance
import Control.DeepSeq
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)
import VCard.Component.Class
import VCard.Merge
import VCard.Property
import VCard.PropertyType
import qualified VCard.V3 as V3

data Card = Card
  { cardSources :: ![Source],
    cardFormattedNames :: !(NonEmpty FormattedName),
    cardName :: !(Maybe Name),
    cardNicknames :: ![Nickname],
    cardGender :: !(Maybe Gender),
    cardEmails :: ![Email],
    cardTelephones :: ![Telephone],
    cardProductIdentifier :: !(Maybe ProductIdentifier),
    cardUID :: !(Maybe UID),
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
    when (cardVersion /= version4) $ unfixableError $ ComponentParseErrorVersionMismatch cardVersion version4

    cardSources <- listOfPropertiesP componentProperties
    cardFormattedNames <- nonemptyListOfPropertiesP componentProperties
    cardName <- optionalPropertyP componentProperties
    cardNicknames <- listOfPropertiesP componentProperties
    cardGender <- optionalPropertyP componentProperties
    cardEmails <- listOfPropertiesP componentProperties
    cardTelephones <- listOfPropertiesP componentProperties
    cardProductIdentifier <- optionalPropertyP componentProperties
    cardUID <- optionalPropertyP componentProperties
    cardRevision <- optionalPropertyP componentProperties
    cardURLs <- listOfPropertiesP componentProperties
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
        listOfPropertiesB cardEmails,
        listOfPropertiesB cardTelephones,
        optionalPropertyB cardProductIdentifier,
        optionalPropertyB cardUID,
        optionalPropertyB cardRevision,
        listOfPropertiesB cardURLs
      ]

fromV3 :: V3.Card -> Card
fromV3 c =
  Card
    { cardSources = V3.cardSources c,
      cardFormattedNames = V3.cardFormattedNames c,
      cardName = Just $ V3.cardName c,
      cardNicknames = V3.cardNicknames c,
      cardEmails = V3.cardEmails c,
      cardGender = Nothing,
      cardTelephones = V3.cardTelephones c,
      cardProductIdentifier = V3.cardProductIdentifier c,
      cardUID = UIDText <$> V3.cardUID c,
      cardRevision = V3.cardRevision c,
      cardURLs = V3.cardURLs c
    }

toV3 :: Card -> V3.Card
toV3 c =
  V3.Card
    { V3.cardSources = cardSources c,
      V3.cardFormattedNames = cardFormattedNames c,
      V3.cardName = fromMaybe emptyName $ cardName c,
      V3.cardNicknames = cardNicknames c,
      V3.cardEmails = cardEmails c,
      V3.cardTelephones = cardTelephones c,
      V3.cardProductIdentifier = cardProductIdentifier c,
      V3.cardUID = do
        u <- cardUID c
        case u of
          UIDText tuid -> Just tuid
          UIDURI uriuid -> Just $ TextUID $ renderURI $ uriUIDValue uriuid, -- TODO keep parameters too
      V3.cardRevision = cardRevision c,
      V3.cardURLs = cardURLs c
    }

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
      cardName = mergeMaybe cardName c1 c2,
      cardNicknames = mergeList cardNicknames c1 c2,
      cardGender = mergeMaybe cardGender c1 c2,
      cardEmails = mergeList cardEmails c1 c2,
      cardTelephones = mergeList cardTelephones c1 c2,
      cardProductIdentifier = mergeMaybe cardProductIdentifier c1 c2,
      cardUID = mergeMaybe cardUID c1 c2,
      cardRevision = mergeMaybe' max cardRevision c1 c2,
      cardURLs = mergeList cardURLs c1 c2
    }
