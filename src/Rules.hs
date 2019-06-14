{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Rules where

import Protolude hiding (force)

import qualified Data.HashMap.Lazy as HashMap
import Data.Text.Unsafe as Text
import Rock

import qualified Builtin
import qualified Domain
import qualified Elaboration
import Error (Error)
import qualified Error
import qualified Evaluation
import qualified Position
import qualified Index
import Monad
import Name (Name(Name))
import qualified Name
import qualified Parser
import qualified Presyntax
import Query
import qualified Resolution
import qualified Scope
import qualified Span
import qualified Syntax

rules :: GenRules (Writer [Error] Query) Query
rules (Writer query) =
  case query of
    ReadFile filePath ->
      noError $ liftIO $ readFile filePath

    ParsedModule module_ -> do
      let
        filePath =
          moduleFilePath module_

      text <- fetch $ ReadFile filePath
      pure $
        case Parser.parseText Parser.module_ text filePath of
          Right errorsAndDefinitions -> do
            let
              (errors, definitions) =
                partitionEithers errorsAndDefinitions
            (definitions, map (Error.Parse filePath) errors)

          Left err ->
            (mempty, pure $ Error.Parse filePath err)

    ParsedModuleMap module_ ->
      noError $ do
        defs <- fetch $ ParsedModule module_
        pure $ HashMap.fromList
          [ ((Presyntax.key def, name), def)
          | (_, (name, def)) <- defs
          ]

    ModulePositionMap module_ ->
      noError $ do
        defs <- fetch $ ParsedModule module_
        pure $
          HashMap.fromList
            [ ((Presyntax.key def, name), loc)
            | (loc, (name, def)) <- defs
            ]

    ParsedDefinition (Scope.KeyedName key (Name.Qualified module_ name)) ->
      noError $ do
        defs <- fetch $ ParsedModuleMap module_
        pure $ HashMap.lookup (key, name) defs

    Scopes module_ -> do
      defs <- fetch $ ParsedModule module_
      pure $ Resolution.moduleScopes module_ $ snd <$> defs

    Visibility (Scope.KeyedName key (Name.Qualified module_ keyName)) (Name.Pre name) ->
      noError $ do
        scopes <- fetch $ Scopes module_
        let
          scope = scopes HashMap.! (keyName, key)

        pure $ HashMap.lookup (Name name) scope

    -- TODO
    ResolvedName _ name
      | name == "Type" -> pure (Just Builtin.typeName, mempty)

    ResolvedName key@(Scope.KeyedName _ (Name.Qualified module_ _)) prename@(Name.Pre name) ->
      noError $ do
        visibility <- fetch $ Query.Visibility key prename
        case visibility of
          Nothing ->
            pure Nothing

          Just _ ->
            pure $ Just $ Name.Qualified module_ (Name name)

    ElaboratedType name
      -- TODO
      | name == Builtin.fail ->
        pure (Syntax.Pi "x" (Syntax.Global Builtin.typeName) $ Syntax.Var Index.Zero, mempty)

      | name == Builtin.typeName ->
        pure (Syntax.Global Builtin.typeName, mempty)

      | otherwise -> do
        let
          key =
            Scope.KeyedName Scope.Type name
        mtype <- fetch $ ParsedDefinition key
        case mtype of
          Nothing -> do
            mdef <- fetch $ ElaboratedDefinition name
            case mdef of
              Nothing ->
                panic "ElaboratedType: No type or definition"

              Just (_, type_) ->
                pure (type_, mempty)

          Just def -> do
            (maybeResult, errs) <- runElaborator key $
              Elaboration.checkDefinition key def Builtin.type_
            pure $
              case maybeResult of
                Nothing ->
                  ( Syntax.App
                    (Syntax.Global Builtin.fail)
                    (Syntax.Global Builtin.typeName)
                  , errs
                  )

                Just (Syntax.TypeDeclaration result) ->
                  (result, errs)

                Just _ ->
                  panic "ElaboratedType: Not a type declaration"

    ElaboratedDefinition name
      -- TODO
      | name == Builtin.fail ->
        pure (Nothing, mempty)

      | name == Builtin.typeName ->
        pure (Nothing, mempty)

      | otherwise -> do
        let
          defKey =
            Scope.KeyedName Scope.Definition name
        mdef <- fetch $ ParsedDefinition defKey
        case mdef of
          Nothing ->
            pure (Nothing, mempty)

          Just def -> do
            let
              typeKey =
                Scope.KeyedName Scope.Type name
            mtype <- fetch $ ParsedDefinition typeKey
            case mtype of
              Nothing ->
                runElaborator defKey $ Elaboration.inferDefinition defKey def

              Just _ -> do
                type_ <- fetch $ ElaboratedType name
                runElaborator defKey $ do
                  typeValue <- Evaluation.evaluate Domain.empty type_
                  (def', errs) <- Elaboration.checkDefinition defKey def typeValue
                  pure ((def', type_), errs)

    ErrorSpan err ->
      noError $
        case err of
          Error.Parse filePath parseError ->
            pure
              ( filePath
              , Span.Absolute (Error.position parseError) (Error.position parseError)
              )

          Error.DuplicateName keyedName ->
            fetch $ KeyedNameSpan keyedName

          Error.Elaboration keyedName (Error.Spanned relativeSpan _) -> do
            (file, Span.Absolute absolutePosition _) <- fetch $ KeyedNameSpan keyedName
            pure (file, Span.add absolutePosition relativeSpan)

    KeyedNameSpan (Scope.KeyedName key (Name.Qualified module_ name@(Name textName))) ->
      noError $ do
        positions <- fetch $ ModulePositionMap module_
        pure
          ( moduleFilePath module_
          , case HashMap.lookup (key, name) positions of
            Nothing ->
              Span.Absolute 0 0

            Just position ->
              Span.Absolute
                position
                (position + Position.Absolute (Text.lengthWord16 textName))
          )
  where
    noError :: Functor m => m a -> m (a, [Error])
    noError = fmap (, mempty)

    runElaborator
      :: Scope.KeyedName
      -> M (a, [Error])
      -> Task Query (Maybe a, [Error])
    runElaborator key m = do
      eitherResult <- runM m
      pure $
        case eitherResult of
          Left err ->
            ( Nothing
            , pure $
              Error.Elaboration key $
              Error.Spanned (Span.Relative 0 0) err
            )

          Right (result, errs) ->
            (Just result, errs)
