{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VCard.Component.Class
  ( ComponentName,
    Component,
    IsComponent (..),
    ComponentParseError (..),
    ComponentParseFixableError (..),
    ComponentParseWarning (..),
    parseGeneralComponent,
    parseGeneralComponents,
    renderGeneralComponent,
    renderGeneralComponents,
    parseComponentFromContentLines,
    namedComponentP,
    namedComponentB,

    -- * Helper functions for writing the parser
    requiredPropertyP,
    optionalPropertyP,
    optionalPropertyWithDefaultP,
    listOfPropertiesP,
    nonemptyListOfPropertiesP,
    setOfPropertiesP,

    -- * Helper functions for writing the builder
    requiredPropertyB,
    optionalPropertyB,
    optionalPropertyWithDefaultB,
    listOfPropertiesB,
    nonemptyListOfPropertiesB,
    setOfPropertiesB,
  )
where

import Conformance
import Control.Exception
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import VCard.ContentLine
import VCard.Property

type ComponentName = Text

type Component = Map ContentLineName (NonEmpty ContentLineValue)

data ComponentParseError
  = ComponentParseErrorMissingBegin
  | ComponentParseErrorMissingEnd !Text
  | ComponentParseErrorIncorrectEnd !Text !Text
  | ComponentParseErrorComponentIncorrectName !Text !Text
  | ComponentParseErrorMissingRequiredProperty !ContentLineName
  | ComponentParseErrorPropertyError !PropertyParseError
  | ComponentParseErrorUnknownVersion !Version
  | ComponentParseErrorVersionMismatch !Version !Version
  deriving (Show, Eq)

instance Exception ComponentParseError where
  displayException = \case
    ComponentParseErrorMissingBegin -> "Tried to parse a component, but didn't find a BEGIN property."
    ComponentParseErrorMissingEnd n ->
      unwords
        [ "Missing END property for component with name",
          show n
        ]
    ComponentParseErrorIncorrectEnd expected actual ->
      unwords
        [ unwords ["Missing END property for component with name", show expected],
          unwords ["found an END property for component with name", show actual, "instead."]
        ]
    ComponentParseErrorComponentIncorrectName actual expected ->
      unwords
        [ "Incorrectly named component, found ",
          show actual,
          "but expected",
          show expected
        ]
    ComponentParseErrorMissingRequiredProperty name ->
      unwords
        [ "Missing required property:",
          show (renderContentLineName name)
        ]
    ComponentParseErrorPropertyError pe -> displayException pe
    ComponentParseErrorUnknownVersion actual ->
      unwords ["Unknown version:", show actual]
    ComponentParseErrorVersionMismatch actual expected ->
      unwords ["Version mismatch. Actual:", show actual, "Expected:", show expected]

data ComponentParseFixableError
  = ComponentParseFixableErrorPropertyFixableError !PropertyParseFixableError
  | ComponentParseFixableErrorMoreThanOneRequiredPropertyValue !ContentLineName !ContentLineValue !ContentLineValue ![ContentLineValue]
  | ComponentParseFixableErrorMoreThanOneOptionalPropertyValue !ContentLineName !ContentLineValue !ContentLineValue ![ContentLineValue]
  | ComponentParseFixableErrorInvalidPropertyOmitted !ContentLine
  deriving (Show, Eq)

instance Exception ComponentParseFixableError where
  displayException = \case
    ComponentParseFixableErrorPropertyFixableError ppfe -> displayException ppfe
    ComponentParseFixableErrorMoreThanOneRequiredPropertyValue name v1 v2 vRest ->
      unlines $ unwords ["Multiple values of required property, guessing the first:", show name] : map show (v1 : v2 : vRest)
    ComponentParseFixableErrorMoreThanOneOptionalPropertyValue name v1 v2 vRest ->
      unlines $ unwords ["Multiple values of optional property, guessing the first:", show name] : map show (v1 : v2 : vRest)
    ComponentParseFixableErrorInvalidPropertyOmitted cl ->
      unwords ["Invalid property omitted:", show cl]

data ComponentParseWarning = UnknownComponent !Text
  deriving (Show, Eq)

instance Exception ComponentParseWarning where
  displayException = \case
    UnknownComponent t ->
      unwords
        [ "Unknown component:",
          show t
        ]

type ConformComponent a =
  Conform
    ComponentParseError
    ComponentParseFixableError
    ComponentParseWarning
    a

renderGeneralComponents :: Map ComponentName (NonEmpty Component) -> DList ContentLine
renderGeneralComponents =
  foldMap
    ( \(name, components) ->
        foldMap (renderGeneralComponent name) (NE.toList components)
    )
    . M.toList

renderGeneralComponent :: Text -> Component -> DList ContentLine
renderGeneralComponent name componentProperties =
  mconcat
    [ DList.singleton $ propertyContentLineB (Begin name),
      DList.fromList $
        concatMap
          ( \(n, values) ->
              map (ContentLine Nothing n) (NE.toList values)
          )
          (M.toList componentProperties),
      DList.singleton $ propertyContentLineB (End name)
    ]

parseGeneralComponents ::
  [ContentLine] ->
  ConformComponent
    (Map ComponentName (NonEmpty Component))
parseGeneralComponents = go
  where
    go ::
      [ContentLine] ->
      ConformComponent (Map ComponentName (NonEmpty Component))
    go = \case
      [] -> pure M.empty
      cls -> do
        ((name, component), leftovers) <- parseGeneralComponentHelper cls
        restComponents <- go leftovers
        pure (M.insertWith (<>) name (component :| []) restComponents)

parseGeneralComponent ::
  [ContentLine] ->
  ConformComponent (Text, Component)
parseGeneralComponent =
  -- TODO check that there were no other lines after this.
  fmap fst . parseGeneralComponentHelper

parseGeneralComponentHelper ::
  [ContentLine] ->
  ConformComponent
    ((ComponentName, Component), [ContentLine])
parseGeneralComponentHelper = \case
  [] -> unfixableError ComponentParseErrorMissingBegin
  (firstCL : restCLs) -> do
    Begin name <-
      conformMapAll
        ComponentParseErrorPropertyError
        ComponentParseFixableErrorPropertyFixableError
        absurd
        $ propertyContentLineP firstCL
    go name M.empty M.empty restCLs
  where
    go ::
      Text ->
      Map ContentLineName (NonEmpty ContentLineValue) ->
      Map ComponentName (NonEmpty Component) ->
      [ContentLine] ->
      ConformComponent
        ((ComponentName, Component), [ContentLine])
    go name properties subComponents = \case
      [] -> unfixableError $ ComponentParseErrorMissingEnd name
      (cl : rest) ->
        case contentLineName cl of
          "END" -> do
            End name' <-
              conformMapAll
                ComponentParseErrorPropertyError
                ComponentParseFixableErrorPropertyFixableError
                absurd
                $ propertyContentLineP
                  cl
            if name' == name
              then
                pure
                  ( ( name,
                      properties
                    ),
                    rest
                  )
              else unfixableError $ ComponentParseErrorIncorrectEnd name name'
          "BEGIN" -> do
            ((name', subComponent), leftovers) <- parseGeneralComponentHelper (cl : rest)
            go
              name
              properties
              (M.insertWith (flip (<>)) name' (subComponent :| []) subComponents)
              leftovers
          _ ->
            go
              name
              (M.insertWith (flip (<>)) (contentLineName cl) (contentLineValue cl :| []) properties)
              subComponents
              rest

parseComponentFromContentLines ::
  (IsComponent component) =>
  [ContentLine] ->
  ConformComponent component
parseComponentFromContentLines cls = do
  parseGeneralComponent cls >>= uncurry namedComponentP

-- |
--
-- === Laws
--
-- * The '[ContentLine]' that is built is valid:
--
-- >>> forAllValid $ \component -> isValid (componentB component)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \contentLines -> isValid (parse componentP "" contentLines)
--
-- * The property roundtrips through '[ContentLine]'.
--
-- >>> forAllValid $ \component -> parse componentP "" (DList.toList (componentB component)) == Right component
class IsComponent component where
  -- | Name for this component
  componentName :: Proxy component -> Text

  -- | Parser for this component
  --
  -- TODO fixable warning for VERSION 4 that's not the first property
  componentP :: Component -> ConformComponent component

  -- | Builder for this component
  --
  -- We need to use a 'DList' instead of a 'Component' because the VERSION property must come first:
  --
  -- @
  -- [Section 6.7.9: VERSION](https://datatracker.ietf.org/doc/html/rfc6350#section-6.7.9)
  --
  -- @
  -- Special notes:  This property MUST be present in the vCard object,
  --    and it must appear immediately after BEGIN:VCARD.  The value MUST
  --    be "4.0" if the vCard corresponds to this specification.  Note
  --    that earlier versions of vCard allowed this property to be placed
  --    anywhere in the vCard object, or even to be absent.
  -- @
  -- @
  componentB :: component -> DList ContentLine

namedComponentP ::
  forall component.
  (IsComponent component) =>
  Text ->
  Component ->
  ConformComponent component
namedComponentP actualName component =
  let expectedName = componentName (Proxy :: Proxy component)
   in if actualName == expectedName
        then componentP component
        else unfixableError $ ComponentParseErrorComponentIncorrectName actualName expectedName

namedComponentB ::
  forall component.
  (IsComponent component) =>
  component ->
  DList ContentLine
namedComponentB component =
  let name = componentName (Proxy :: Proxy component)
   in mconcat
        [ DList.singleton $ propertyContentLineB $ Begin name,
          componentB component,
          DList.singleton $ propertyContentLineB $ End name
        ]

requiredPropertyP ::
  forall a.
  (IsProperty a) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  ConformComponent a
requiredPropertyP m = case M.lookup name m of
  Nothing -> unfixableError $ ComponentParseErrorMissingRequiredProperty name
  Just (value :| restValues) -> do
    case NE.nonEmpty restValues of
      Nothing -> pure ()
      Just (secondValue :| lastValues) ->
        emitFixableError $ ComponentParseFixableErrorMoreThanOneRequiredPropertyValue name value secondValue lastValues
    conformMapAll
      ComponentParseErrorPropertyError
      ComponentParseFixableErrorPropertyFixableError
      absurd
      $ propertyContentLineP (ContentLine Nothing name value)
  where
    name = propertyName (Proxy :: Proxy a)

requiredPropertyB :: (IsProperty property) => property -> DList ContentLine
requiredPropertyB = DList.singleton . propertyContentLineB

optionalPropertyB :: (IsProperty property) => Maybe property -> DList ContentLine
optionalPropertyB = maybe DList.empty requiredPropertyB

optionalPropertyWithDefaultB ::
  (Eq property, IsProperty property) =>
  property ->
  property ->
  DList ContentLine
optionalPropertyWithDefaultB defaultValue value =
  if value == defaultValue
    then mempty
    else requiredPropertyB value

optionalPropertyP ::
  forall a.
  ( Validity a,
    IsProperty a
  ) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  ConformComponent (Maybe a)
optionalPropertyP m = case M.lookup name m of
  Nothing -> pure Nothing
  Just (value :| restValues) -> do
    case NE.nonEmpty restValues of
      Nothing -> pure ()
      Just (secondValue :| lastValues) ->
        emitFixableError $ ComponentParseFixableErrorMoreThanOneOptionalPropertyValue name value secondValue lastValues
    mPValue <-
      tryConform
        $ conformMapAll
          ComponentParseErrorPropertyError
          ComponentParseFixableErrorPropertyFixableError
          absurd
        $ propertyContentLineP (ContentLine Nothing name value)
    case mPValue of
      Just pValue | isValid pValue -> pure (Just pValue)
      _ -> do
        emitFixableError $ ComponentParseFixableErrorInvalidPropertyOmitted (ContentLine Nothing name value)
        pure Nothing
  where
    name = propertyName (Proxy :: Proxy a)

optionalPropertyWithDefaultP ::
  forall a.
  (Validity a, IsProperty a) =>
  a ->
  Map ContentLineName (NonEmpty ContentLineValue) ->
  ConformComponent a
optionalPropertyWithDefaultP defaultValue m = fromMaybe defaultValue <$> optionalPropertyP m

listOfPropertiesB ::
  (IsProperty property) =>
  [property] ->
  DList ContentLine
listOfPropertiesB = foldMap requiredPropertyB

listOfPropertiesP ::
  forall property.
  (IsProperty property) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  ConformComponent [property]
listOfPropertiesP m = do
  let values = maybe [] NE.toList $ M.lookup name m
  mapM
    ( conformMapAll
        ComponentParseErrorPropertyError
        ComponentParseFixableErrorPropertyFixableError
        absurd
        . propertyContentLineP
    )
    (map (ContentLine Nothing name) values)
  where
    name = propertyName (Proxy :: Proxy property)

nonemptyListOfPropertiesB ::
  (IsProperty property) =>
  NonEmpty property ->
  DList ContentLine
nonemptyListOfPropertiesB = listOfPropertiesB . NE.toList

nonemptyListOfPropertiesP ::
  forall property.
  (IsProperty property) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  ConformComponent (NonEmpty property)
nonemptyListOfPropertiesP m =
  case M.lookup name m of
    Nothing -> unfixableError $ ComponentParseErrorMissingRequiredProperty name
    Just values ->
      mapM
        ( conformMapAll
            ComponentParseErrorPropertyError
            ComponentParseFixableErrorPropertyFixableError
            absurd
            . propertyContentLineP
        )
        (NE.map (ContentLine Nothing name) values)
  where
    name = propertyName (Proxy :: Proxy property)

setOfPropertiesB ::
  (IsProperty property) =>
  Set property ->
  DList ContentLine
setOfPropertiesB = listOfPropertiesB . S.toList

setOfPropertiesP ::
  forall property.
  (Ord property, IsProperty property) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  ConformComponent (Set property)
setOfPropertiesP = fmap S.fromList . listOfPropertiesP
