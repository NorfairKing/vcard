{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Parameter
  ( module VCard.Parameter,
    module VCard.Parameter.Class,
    module VCard.Parameter.ValueDataType,
  )
where

import Conformance
import Control.DeepSeq
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.String
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import VCard.ContentLine
import VCard.Parameter.Class
import VCard.Parameter.ValueDataType
