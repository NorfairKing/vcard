{-# LANGUAGE DeriveGeneric #-}

module VCard
  ( VCard (..),
  )
where

import Control.DeepSeq
import Data.Validity
import GHC.Generics (Generic)

data VCard = VCard
  deriving (Show, Generic)

instance Validity VCard

instance NFData VCard
