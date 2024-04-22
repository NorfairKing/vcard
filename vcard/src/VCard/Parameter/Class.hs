{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VCard.Parameter.Class
  ( ParameterParseError,
    ParameterParseFixableError,
    ParameterParseWarning,
    IsParameter (..),

    -- ** Parsing
    optionalParam,

    -- ** Rendering
    insertParam,
  )
where

import Conformance
import Control.Exception
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Validity.Time ()
import Data.Void
import Text.Megaparsec
import VCard.ContentLine

deriving instance (Ord s) => Ord (PosState s)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseError s e)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseErrorBundle s e)

type ParameterParseError = Void

data ParameterParseFixableError
  = MultipleParametersfound !(NonEmpty ParamValue)
  deriving (Show, Eq, Ord)

instance Exception ParameterParseFixableError where
  displayException = \case
    MultipleParametersfound values ->
      unlines
        [ "Multiple parameter values found where one was expected.",
          "values:",
          show values
        ]

type ParameterParseWarning = Void

type ConformParameter a = Conform ParameterParseError ParameterParseFixableError ParameterParseWarning a

-- | Parameters
--
-- === Laws
--
-- * The 'NonEmpty ParamValue' that is built is valid:
--
-- >>> forAllValid $ \parameter -> isValid (parameterB parameter)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \paramValue -> isValid (parameterP paramValue)
--
-- * The parameter roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \parameter -> runConformStrict (parameterP (parameterB parameter)) == Right parameter
class IsParameter param where
  -- Name of the parameter
  parameterName :: Proxy param -> ParamName

  -- | Parser for the parameter
  parameterP :: ParamValue -> Conform ParameterParseError ParameterParseFixableError Void param

  -- | Builder for the parameter
  parameterB :: param -> ParamValue

optionalParam ::
  forall param.
  (IsParameter param) =>
  Map ParamName (NonEmpty ParamValue) ->
  ConformParameter (Maybe param)
optionalParam m = do
  let name = parameterName (Proxy :: Proxy param)
  forM (M.lookup name m) $ \ne@(paramValue :| rest) -> do
    when (not (null rest)) $ emitFixableError $ MultipleParametersfound ne
    parameterP paramValue

insertParam :: forall param. (IsParameter param) => param -> ContentLineValue -> ContentLineValue
insertParam param clv =
  clv
    { contentLineValueParams =
        M.insert (parameterName (Proxy :: Proxy param)) (parameterB param :| []) (contentLineValueParams clv)
    }
