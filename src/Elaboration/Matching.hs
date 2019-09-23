{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language RankNTypes #-}
module Elaboration.Matching where

import Protolude hiding (IntMap, IntSet, force)

import Control.Monad.Fail
import Control.Monad.Trans.Maybe
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Rock

import {-# source #-} qualified Elaboration
import "this" Data.IntMap (IntMap)
import Binding (Binding)
import qualified Binding
import qualified Builtin
import Context (Context)
import qualified Context
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import qualified Domain
import Domain.Pattern (Pattern)
import qualified Domain.Pattern as Pattern
import qualified Domain.Telescope as Domain (Telescope)
import qualified Domain.Telescope
import qualified Elaboration.Matching.SuggestedName as SuggestedName
import qualified Environment
import qualified Error
import qualified Evaluation
import qualified Flexibility
import Monad
import Name (Name(Name))
import qualified Name
import Plicity
import qualified Presyntax
import qualified "this" Data.IntMap as IntMap
import qualified Query
import qualified Readback
import qualified Scope
import qualified Span
import qualified Syntax
import Syntax.Telescope (Telescope)
import qualified Syntax.Telescope as Telescope
import qualified Unification
import qualified Unification.Indices as Indices
import Var

data Config = Config
  { _expectedType :: !Domain.Value
  , _scrutinees :: ![(Plicity, Domain.Value)]
  , _clauses :: [Clause]
  , _usedClauses :: !(IORef (Set Span.Relative))
  , _coveredConstructors :: CoveredConstructors
  , _matchKind :: !Error.MatchKind
  }

type CoveredConstructors = IntMap Var (HashSet Name.QualifiedConstructor)

data Clause = Clause
  { _span :: !Span.Relative
  , _matches :: [Match]
  , _rhs :: !Presyntax.Term
  }

data Match = Match !Domain.Value !Domain.Value !Plicity !Presyntax.Pattern !Domain.Type

-------------------------------------------------------------------------------

elaborateCase
  :: Context v
  -> Syntax.Term v
  -> Domain.Type
  -> [(Presyntax.Pattern, Presyntax.Term)]
  -> Domain.Type
  -> M (Syntax.Term v)
elaborateCase context scrutinee scrutineeType branches expectedType = do
  usedClauses <- liftIO $ newIORef mempty
  scrutineeValue <- Elaboration.evaluate context scrutinee
  isPatternScrutinee <- isPatternValue context scrutineeValue

  (context', var) <-
    if isPatternScrutinee then
      Context.extendUnnamedDef context "scrutinee" scrutineeValue scrutineeType
    else
      Context.extendUnnamed context "scrutinee" scrutineeType

  let
    scrutineeVarValue =
      Domain.var var
  term <- elaborateWithCoverage context' Config
    { _expectedType = expectedType
    , _scrutinees = pure (Explicit, scrutineeVarValue)
    , _clauses =
      [ Clause
        { _span = Span.add patSpan rhsSpan
        , _matches = [Match scrutineeVarValue scrutineeVarValue Explicit pat scrutineeType]
        , _rhs = rhs'
        }
      | (pat@(Presyntax.Pattern patSpan _), rhs'@(Presyntax.Term rhsSpan _)) <- branches
      ]
    , _usedClauses = usedClauses
    , _coveredConstructors = mempty
    , _matchKind = Error.Branch
    }
  scrutineeType' <- Readback.readback (Context.toEnvironment context) scrutineeType
  pure $ Syntax.Let "scrutinee" scrutinee scrutineeType' term

isPatternValue :: Context v -> Domain.Value -> M Bool
isPatternValue context value = do
  value' <- Context.forceHead context value
  case value' of
    Domain.Neutral (Domain.Var _) Tsil.Empty ->
      pure True

    Domain.Neutral (Domain.Var _) (_ Tsil.:> _) ->
      pure False

    Domain.Neutral (Domain.Global _) _ ->
      pure False

    Domain.Neutral (Domain.Con constr) spine -> do
      constrTypeTele <- fetch $ Query.ConstructorType constr
      let
        spine' =
          dropTypeArgs constrTypeTele $ toList spine

      and <$> mapM (isPatternValue context . snd) spine'

    Domain.Neutral (Domain.Meta _) _ ->
      pure False

    Domain.Glued _ _ value'' -> do
      value''' <- force value''
      isPatternValue context value'''

    Domain.Lam {} ->
      pure False

    Domain.Pi {} ->
      pure False

    Domain.Fun {} ->
      pure False

    Domain.Case {} ->
      pure False

  where
    dropTypeArgs
      :: Telescope t t' v
      -> [(Plicity, value)]
      -> [(Plicity, value)]
    dropTypeArgs tele args =
      case (tele, args) of
        (Telescope.Empty _, _) ->
          args

        (Telescope.Extend _ _ plicity1 tele', (plicity2, _):args')
          | plicity1 == plicity2 ->
            dropTypeArgs tele' args'

        _ ->
          panic "chooseBranch arg mismatch"

elaborateClauses
  :: Context v
  -> [Clause]
  -> Domain.Type
  -> M (Syntax.Term v)
elaborateClauses context clauses expectedType = do
  usedClauses <- liftIO $ newIORef mempty

  elaborateWithCoverage context Config
    { _expectedType = expectedType
    , _scrutinees =
      case clauses of
        firstClause:_ ->
          [(plicity, value) | Match value _ plicity _ _ <- _matches firstClause]

        _ ->
          mempty

    , _clauses = clauses
    , _usedClauses = usedClauses
    , _coveredConstructors = mempty
    , _matchKind = Error.Clause
    }

elaborateSingle
  :: Context v
  -> Var
  -> Plicity
  -> Presyntax.Pattern
  -> Presyntax.Term
  -> Domain.Type
  -> M (Syntax.Term v)
elaborateSingle context scrutinee plicity pat@(Presyntax.Pattern patSpan _) rhs@(Presyntax.Term rhsSpan _) expectedType = do
    let
      scrutineeValue =
        Domain.var scrutinee

      scrutineeType =
        Context.lookupVarType scrutinee context

    usedClauses <- liftIO $ newIORef mempty

    elaborateWithCoverage context Config
      { _expectedType = expectedType
      , _scrutinees = pure (plicity, scrutineeValue)
      , _clauses =
        [ Clause
          { _span = Span.add patSpan rhsSpan
          , _matches = [Match scrutineeValue scrutineeValue plicity pat scrutineeType]
          , _rhs = rhs
          }
        ]
      , _usedClauses = usedClauses
      , _coveredConstructors = mempty
      , _matchKind = Error.Lambda
      }

-------------------------------------------------------------------------------

elaborateWithCoverage :: Context v -> Config -> M (Syntax.Term v)
elaborateWithCoverage context config = do
  result <- elaborate context config
  let
    allClauseSpans =
      Set.fromList
        [ span
        | Clause span _ _ <- _clauses config
        ]
  usedClauseSpans <- liftIO $ readIORef (_usedClauses config)
  forM_ (Set.difference allClauseSpans usedClauseSpans) $ \span ->
    Context.report (Context.spanned span context) $ Error.RedundantMatch $ _matchKind config
  pure result

elaborate :: Context v -> Config -> M (Syntax.Term v)
elaborate context config = do
  clauses <- catMaybes <$> mapM (simplifyClause context $ _coveredConstructors config) (_clauses config)
  let
    config' = config { _clauses = clauses }
  case clauses of
    [] -> do
      exhaustive <- anyM (uninhabitedScrutinee context (_coveredConstructors config) . snd) $ _scrutinees config
      unless exhaustive $ do
        scrutinees <- forM (_scrutinees config) $ \(plicity, scrutinee) -> do
          patterns <- uncoveredScrutineePatterns context (_coveredConstructors config) scrutinee
          pure $ (,) plicity <$> (Context.toPrettyablePattern context <$> patterns)
        Context.report context $ Error.NonExhaustivePatterns $ sequence scrutinees
      targetType <- Elaboration.readback context $ _expectedType config
      pure $ Syntax.App (Syntax.Global Builtin.fail) Explicit targetType

    firstClause:_ -> do
      let
        matches = _matches firstClause

      splitEqualityOr context config' matches $
        splitConstructorOr context config' matches $ do
          maybeInst <- solved context matches
          case maybeInst of
            Nothing -> do
             Context.report context $ Error.IndeterminateIndexUnification $ _matchKind config
             targetType <- Elaboration.readback context $ _expectedType config
             pure $ Syntax.App (Syntax.Global Builtin.fail) Explicit targetType

            Just inst -> do
              context' <- Context.extendUnindexedDefs context inst
              mapM_ (checkForcedPattern context') matches
              result <- Elaboration.check context' (_rhs firstClause) (_expectedType config)
              liftIO $ modifyIORef (_usedClauses config) $ Set.insert $ _span firstClause
              pure result

checkForcedPattern :: Context v -> Match -> M ()
checkForcedPattern context match =
  case match of
    Match value1 _ _ (Presyntax.Pattern span (Presyntax.Forced term)) type_ -> do
      let
        context' =
          Context.spanned span context

      term' <- Elaboration.check context' term type_
      value2 <- Elaboration.evaluate context term'
      _ <- Context.try_ context' $ Unification.unify context' Flexibility.Rigid value1 value2
      pure ()

    _ ->
      pure ()

uncoveredScrutineePatterns
  :: Context v
  -> CoveredConstructors
  -> Domain.Value
  -> M [Pattern]
uncoveredScrutineePatterns context coveredConstructors value = do
  value' <- Context.forceHead context value
  case value' of
    Domain.Neutral (Domain.Var v) Tsil.Empty -> do
      let
        covered =
          IntMap.lookupDefault mempty v coveredConstructors

        go :: Name.Qualified -> Telescope Syntax.Type Syntax.ConstructorDefinitions v -> [Pattern]
        go typeName tele =
          case tele of
            Telescope.Empty (Syntax.ConstructorDefinitions constrDefs) -> do
              let
                uncoveredConstrDefs =
                  HashMap.difference
                    constrDefs
                    (HashSet.toMap $
                      HashSet.map (\(Name.QualifiedConstructor _ constr) -> constr) covered
                    )

              foreach (HashMap.toList uncoveredConstrDefs) $ \(constr, type_) ->
                Pattern.Con
                  (Name.QualifiedConstructor typeName constr)
                  [ (plicity, Pattern.Wildcard)
                  | plicity <- Syntax.constructorFieldPlicities type_
                  ]

            Telescope.Extend _ _ _ tele' ->
              go typeName tele'

      case HashSet.toList covered of
        [] ->
          pure [Pattern.Wildcard]

        Name.QualifiedConstructor typeName _:_ -> do
          maybeDefinition <- fetch $ Query.ElaboratedDefinition typeName
          case maybeDefinition of
            Just (Syntax.DataDefinition tele, _) ->
              pure $ go typeName tele

            _ ->
              panic "uncoveredScrutineePatterns non-data"

    Domain.Neutral (Domain.Var _) (_ Tsil.:> _) ->
      pure []

    Domain.Neutral (Domain.Global _) _ ->
      pure []

    Domain.Neutral (Domain.Con constr) spine -> do
      constrTypeTele <- fetch $ Query.ConstructorType constr
      let
        spine' =
          dropTypeArgs constrTypeTele $ toList spine

      spine'' <- forM spine' $ \(plicity, arg) -> do
        patterns <- uncoveredScrutineePatterns context coveredConstructors arg
        pure $ (,) plicity <$> patterns
      pure $ Pattern.Con constr <$> sequence spine''

    Domain.Neutral (Domain.Meta _) _ ->
      pure []

    Domain.Glued _ _ value'' -> do
      value''' <- force value''
      uncoveredScrutineePatterns context coveredConstructors value'''

    Domain.Lam {} ->
      pure []

    Domain.Pi {} ->
      pure []

    Domain.Fun {} ->
      pure []

    Domain.Case {} ->
      pure []
  where
    dropTypeArgs
      :: Telescope t t' v
      -> [(Plicity, value)]
      -> [(Plicity, value)]
    dropTypeArgs tele args =
      case (tele, args) of
        (Telescope.Empty _, _) ->
          args

        (Telescope.Extend _ _ plicity1 tele', (plicity2, _):args')
          | plicity1 == plicity2 ->
            dropTypeArgs tele' args'

        _ ->
          panic "chooseBranch arg mismatch"

-------------------------------------------------------------------------------

simplifyClause :: Context v -> CoveredConstructors -> Clause -> M (Maybe Clause)
simplifyClause context coveredConstructors clause = do
  maybeMatches <- runMaybeT $
    concat <$> mapM (simplifyMatch context coveredConstructors) (_matches clause)
  case maybeMatches of
    Nothing ->
      pure Nothing

    Just matches' -> do
      maybeExpanded <- runMaybeT $ expandAnnotations context matches'
      case maybeExpanded of
        Nothing ->
          pure $ Just clause { _matches = matches' }

        Just expandedMatches ->
          simplifyClause context coveredConstructors clause { _matches = expandedMatches }

simplifyMatch
  :: Context v
  -> CoveredConstructors
  -> Match
  -> MaybeT M [Match]
simplifyMatch context coveredConstructors (Match value forcedValue plicity pat@(Presyntax.Pattern span unspannedPattern) type_) = do
  forcedValue' <- lift $ Context.forceHead context forcedValue
  let
    match' =
      Match value forcedValue' plicity pat type_
  case (forcedValue', unspannedPattern) of
    (Domain.Neutral (Domain.Con constr) spine, Presyntax.ConOrVar _ name pats) -> do
      maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
      case maybeScopeEntry of
        Just scopeEntry
          | constr `HashSet.member` Scope.entryConstructors scopeEntry -> do
            matches' <- lift $ do
              constrType <- fetch $ Query.ConstructorType constr
              (patsType, patSpine) <-
                instantiateConstructorType
                  (Context.toEnvironment context)
                  (Telescope.fromVoid constrType)
                  (toList spine)

              (matches', type') <- matchPrepatterns context patSpine pats patsType
              let
                context' =
                  Context.spanned span context
              _ <- Context.try_ context' $ Unification.unify context' Flexibility.Rigid type_ type'
              pure matches'
            concat <$> mapM (simplifyMatch context coveredConstructors) matches'

          | otherwise ->
            fail "Constructor mismatch"

        _ ->
          pure [match']

    (Domain.Neutral (Domain.Var var) Tsil.Empty, Presyntax.ConOrVar _ name _)
      | Just coveredConstrs <- IntMap.lookup var coveredConstructors -> do
        maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
        case maybeScopeEntry of
          Just scopeEntry -> do
            let
              expectedTypeName =
                Elaboration.getExpectedTypeName context type_

              entryConstrs =
                Scope.entryConstructors scopeEntry
            resolved <- lift $ Elaboration.resolveConstructor entryConstrs expectedTypeName
            case resolved of
              Elaboration.Ambiguous _ ->
                pure [match']

              Elaboration.Resolved constr
                | HashSet.member constr coveredConstrs ->
                  fail "Constructor already covered"

                | otherwise ->
                  pure [match']

          _ ->
            pure [match']

    _ ->
      pure [match']

instantiateConstructorType
  :: Domain.Environment v
  -> Telescope Syntax.Type Syntax.Type v
  -> [(Plicity, Domain.Value)]
  -> M (Domain.Type, [(Plicity, Domain.Value)])
instantiateConstructorType env tele spine =
  case (tele, spine) of
    (Telescope.Empty constrType, _) -> do
      constrType' <- Evaluation.evaluate env constrType
      pure (constrType', spine)

    (Telescope.Extend _ _ plicity1 tele', (plicity2, arg):spine')
      | plicity1 == plicity2 -> do
        env' <- Environment.extendValue env arg
        instantiateConstructorType env' tele' spine'

    _ ->
      panic $ "instantiateConstructorType: " <> show (tele, fst <$> spine)

matchPrepatterns
  :: Context v
  -> [(Plicity, Domain.Value)]
  -> [Presyntax.PlicitPattern]
  -> Domain.Type
  -> M ([Match], Domain.Type)
matchPrepatterns context values patterns type_ =
  case (patterns, values) of
    ([], []) ->
      pure ([], type_)

    (Presyntax.ExplicitPattern pat:patterns', (Explicit, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi _ source Explicit domainClosure -> do
          domain <- Evaluation.evaluateClosure domainClosure value
          explicitFunCase value values' pat patterns' source domain

        Domain.Fun source domain ->
          explicitFunCase value values' pat patterns' source domain

        _ ->
          panic "matchPrepatterns explicit non-pi"

    (Presyntax.ImplicitPattern _ namedPats:patterns', _)
      | HashMap.null namedPats ->
        matchPrepatterns context values patterns' type_

    (Presyntax.ImplicitPattern patSpan namedPats:patterns', (Implicit, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi name source Implicit domainClosure
          | HashMap.member name namedPats -> do
            domain <- Evaluation.evaluateClosure domainClosure value
            (matches, type'') <-
              matchPrepatterns
                context
                values'
                (Presyntax.ImplicitPattern patSpan (HashMap.delete name namedPats) : patterns')
                domain
            pure (Match value value Implicit (namedPats HashMap.! name) source : matches, type'')

          | otherwise -> do
            domain <- Evaluation.evaluateClosure domainClosure value
            matchPrepatterns context values' patterns domain

        _ ->
          panic "matchPrepatterns implicit non-pi"

    (_, (Implicit, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi _ _ Implicit domainClosure -> do
          domain <- Evaluation.evaluateClosure domainClosure value
          matchPrepatterns context values' patterns domain

        _ ->
          panic "matchPrepatterns implicit non-pi 2"

    (_, (Constraint, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi _ source Constraint domainClosure -> do
          domain <- Evaluation.evaluateClosure domainClosure value
          (matches, type'') <-
            matchPrepatterns
              context
              values'
              patterns
              domain
          let
            pattern_ =
              Presyntax.Pattern (Context.span context) Presyntax.WildcardPattern
          pure (Match value value Constraint pattern_ source : matches, type'')

        _ ->
          panic "matchPrepatterns constraint non-pi"

    (pat:_, []) -> do
      Context.report (Context.spanned (Presyntax.plicitPatternSpan pat) context) $ Error.PlicityMismatch Error.Field Error.Extra
      pure ([], type_)

    ([], (Explicit, _):_) -> do
      Context.report context $ Error.PlicityMismatch Error.Field $ Error.Missing Explicit
      matchPrepatterns context values [Presyntax.ExplicitPattern $ Presyntax.Pattern (Context.span context) Presyntax.WildcardPattern] type_

    (Presyntax.ImplicitPattern patSpan _:patterns', (Explicit, _):_) -> do
      Context.report (Context.spanned patSpan context) $ Error.PlicityMismatch Error.Field (Error.Mismatch Explicit Implicit)
      matchPrepatterns context values patterns' type_

  where
    explicitFunCase value values' pat patterns' source domain = do
      (matches, type'') <- matchPrepatterns context values' patterns' domain
      pure (Match value value Explicit pat source : matches, type'')

type PatternInstantiation = Tsil (Binding, Domain.Value, Domain.Value)

expandAnnotations
  :: Context v
  -> [Match]
  -> MaybeT M [Match]
expandAnnotations context matches =
  case matches of
    [] ->
      fail "expanded nothing"

    match:matches' -> do
      maybeInst <- lift $ runMaybeT $ matchInstantiation context match
      case maybeInst of
        Just inst -> do
          context' <- lift $ Context.extendUnindexedDefs context inst
          matches'' <- expandAnnotations context' matches'
          pure $ match : matches''

        Nothing ->
          case match of
            Match value forcedValue plicity (Presyntax.Pattern span (Presyntax.Anno pat annoType)) type_ -> do
              lift $ do
                annoType' <- Elaboration.check context annoType Builtin.type_
                annoType'' <- Elaboration.evaluate context annoType'
                let
                  context' =
                    Context.spanned span context
                _ <- Context.try_ context' $ Unification.unify context' Flexibility.Rigid annoType'' type_
                pure ()
              pure $ Match value forcedValue plicity pat type_ : matches'

            _ ->
              fail "couldn't create instantitation for prefix"

matchInstantiation :: Context v -> Match -> MaybeT M PatternInstantiation
matchInstantiation context match =
  case match of
    (Match _ _ _ (Presyntax.Pattern _ Presyntax.WildcardPattern) _) ->
      pure mempty

    (Match value _ _ (Presyntax.Pattern span (Presyntax.ConOrVar _ prename@(Name.Pre name) [])) type_) -> do
      maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) prename
      if HashSet.null $ foldMap Scope.entryConstructors maybeScopeEntry then
        pure $ pure (Binding.Spanned span $ Name name, value, type_)

      else
        fail "No match instantiation"

    (Match _ _ _ (Presyntax.Pattern _ (Presyntax.Forced _)) _) ->
      pure mempty

    _ ->
      fail "No match instantitation"

solved :: Context v -> [Match] -> M (Maybe PatternInstantiation)
solved context =
  runMaybeT . fmap mconcat . traverse (matchInstantiation context)

-------------------------------------------------------------------------------

splitConstructorOr
  :: Context v
  -> Config
  -> [Match]
  -> M (Syntax.Term v)
  -> M (Syntax.Term v)
splitConstructorOr context config matches k =
  case matches of
    [] ->
      k

    match:matches' ->
      case match of
        Match
          scrutinee
          (Domain.Neutral (Domain.Var var) Tsil.Empty)
          _
          (Presyntax.Pattern span (Presyntax.ConOrVar _ name _))
          type_ -> do
            maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
            case maybeScopeEntry of
              Just scopeEntry -> do
                let
                  expectedTypeName =
                    Elaboration.getExpectedTypeName context type_

                  entryConstrs =
                    Scope.entryConstructors scopeEntry

                resolved <- Elaboration.resolveConstructor entryConstrs expectedTypeName
                case resolved of
                  Elaboration.Ambiguous _ ->
                    splitConstructorOr context config matches' k

                  Elaboration.Resolved constr ->
                    splitConstructor context config scrutinee var span constr type_

              _ ->
                splitConstructorOr context config matches' k

        _ ->
          splitConstructorOr context config matches' k

splitConstructor
  :: Context v
  -> Config
  -> Domain.Value
  -> Var
  -> Span.Relative
  -> Name.QualifiedConstructor
  -> Domain.Type
  -> M (Syntax.Term v)
splitConstructor outerContext config scrutineeValue scrutineeVar span (Name.QualifiedConstructor typeName _) outerType = do
  maybeDefinition <- fetch $ Query.ElaboratedDefinition typeName
  case maybeDefinition of
    Just (Syntax.DataDefinition tele, _) -> do
      tele' <- Evaluation.evaluateConstructorDefinitions (Environment.empty $ Context.scopeKey outerContext) tele
      outerType' <- Context.forceHead outerContext outerType
      case outerType' of
        Domain.Neutral (Domain.Global typeName') spine
          | typeName == typeName' ->
            goParams (Context.spanned span outerContext) (toList spine) mempty tele'

        _ -> do
          typeType <- fetch $ Query.ElaboratedType typeName
          typeType' <- Evaluation.evaluate (Environment.empty $ Context.scopeKey outerContext) typeType
          (metas, _) <- Elaboration.insertMetas outerContext Elaboration.UntilTheEnd typeType'
          f <- Unification.tryUnify outerContext (Domain.Neutral (Domain.Global typeName) $ Tsil.fromList metas) outerType
          result <- goParams (Context.spanned span outerContext) metas mempty tele'
          pure $ f result

    _ ->
      panic "splitConstructor no data definition"
  where
    goParams
      :: Context v
      -> [(Plicity, Domain.Value)]
      -> Domain.Spine
      -> Domain.Telescope Domain.Type (HashMap Name.Constructor Domain.Type)
      -> M (Syntax.Type v)
    goParams context params conArgs dataTele =
      case (params, dataTele) of
        ([], Domain.Telescope.Empty constructors) -> do
          matchedConstructors <-
            HashMap.fromListWith (<>) . concat . takeWhile (not . null) <$>
              mapM
                (findVarConstructorMatches context scrutineeVar . _matches)
                (_clauses config)

          branches <- flip HashMap.traverseWithKey matchedConstructors $ \qualifiedConstr@(Name.QualifiedConstructor _ constr) patterns -> do
            let
              constrType =
                HashMap.lookupDefault
                  (panic "Matching constrType")
                  constr
                  constructors

              conSpan =
                case patterns of
                  (s, _):_ ->
                    s

                  _ ->
                    Span.Relative 0 0

            tele <- goConstrFields context qualifiedConstr conArgs constrType $ snd <$> patterns
            pure (conSpan, tele)


          defaultBranch <-
            if HashMap.size matchedConstructors == length constructors then
              pure Nothing

            else
              Just <$> elaborate context config
                { _coveredConstructors =
                  IntMap.insertWith (<>) scrutineeVar (HashSet.fromMap $ void matchedConstructors) $
                  _coveredConstructors config
                }

          scrutinee <- Elaboration.readback context scrutineeValue

          pure $ Syntax.Case scrutinee branches defaultBranch

        ((plicity1, param):params', Domain.Telescope.Extend _ _ plicity2 domainClosure)
          | plicity1 == plicity2 -> do
            domain <- domainClosure param
            goParams context params' (conArgs Tsil.:> (implicitise plicity1, param)) domain

        _ ->
          panic "goParams mismatch"

    goConstrFields
      :: Context v
      -> Name.QualifiedConstructor
      -> Domain.Spine
      -> Domain.Type
      -> [[Presyntax.PlicitPattern]]
      -> M (Telescope Syntax.Type Syntax.Term v)
    goConstrFields context constr conArgs type_ patterns =
      case type_ of
        Domain.Pi piName source plicity domainClosure -> do
          source'' <- Elaboration.readback context source
          (name, patterns') <-
            case plicity of
              Explicit ->
                SuggestedName.nextExplicit context patterns

              Implicit ->
                SuggestedName.nextImplicit context piName patterns

              Constraint ->
                pure (Binding.Unspanned piName, patterns)

          (context' , fieldVar) <- Context.extendBefore context scrutineeVar name source
          let
            fieldValue =
              Domain.var fieldVar

            conArgs' =
              conArgs Tsil.:> (plicity, fieldValue)

          domain <- Evaluation.evaluateClosure domainClosure fieldValue
          tele <- goConstrFields context' constr conArgs' domain patterns'
          pure $ Telescope.Extend name source'' plicity tele

        Domain.Fun source domain -> do
          source'' <- Elaboration.readback context source
          (name, patterns') <- SuggestedName.nextExplicit context patterns
          (context' , fieldVar) <- Context.extendBefore context scrutineeVar name source
          let
            fieldValue =
              Domain.var fieldVar

            conArgs' =
              conArgs Tsil.:> (Explicit, fieldValue)

          tele <- goConstrFields context' constr conArgs' domain patterns'
          pure $ Telescope.Extend name source'' Explicit tele

        _ -> do
          let
            context' =
              Context.defineWellOrdered context scrutineeVar $ Domain.Neutral (Domain.Con constr) conArgs
          result <- elaborate context' config
          pure $ Telescope.Empty result

findVarConstructorMatches
  :: Context v
  -> Var
  -> [Match]
  -> M [(Name.QualifiedConstructor, [(Span.Relative, [Presyntax.PlicitPattern])])]
findVarConstructorMatches context var matches =
    case matches of
      [] ->
        pure []

      Match _ (Domain.Neutral (Domain.Var var') Tsil.Empty) _ (Presyntax.Pattern _ (Presyntax.ConOrVar span name patterns)) type_:matches'
        | var == var' -> do
          maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
          case maybeScopeEntry of
            Just scopeEntry -> do
              let
                expectedTypeName =
                  Elaboration.getExpectedTypeName context type_

                entryConstrs =
                  Scope.entryConstructors scopeEntry
              resolved <- Elaboration.resolveConstructor entryConstrs expectedTypeName
              case resolved of
                Elaboration.Ambiguous _ ->
                  findVarConstructorMatches context var matches'

                Elaboration.Resolved constr ->
                  ((constr, [(span, patterns)]) :) <$> findVarConstructorMatches context var matches'

            _ ->
              findVarConstructorMatches context var matches'

      _:matches' ->
        findVarConstructorMatches context var matches'

-------------------------------------------------------------------------------

splitEqualityOr
  :: Context v
  -> Config
  -> [Match]
  -> M (Syntax.Term v)
  -> M (Syntax.Term v)
splitEqualityOr context config matches k =
  case matches of
    [] ->
      k

    match:matches' ->
      case match of
        Match
          _
          (Domain.Neutral (Domain.Var var) Tsil.Empty)
          _
          (Presyntax.Pattern _ Presyntax.WildcardPattern)
          (Builtin.Equals type_ value1 value2) -> do
            result <- runExceptT $ Indices.unify context Flexibility.Rigid mempty value1 value2
            case result of
              Left Indices.Nope ->
                elaborate context config
                  { _clauses = drop 1 $ _clauses config
                  }

              Left Indices.Dunno ->
                splitEqualityOr context config matches' k

              Right context' -> do
                context'' <- Context.define context' var $ Builtin.Refl type_ value1 value2
                elaborate context'' config

        _ ->
          splitEqualityOr context config matches' k

-------------------------------------------------------------------------------

uninhabitedScrutinee :: Context v -> CoveredConstructors -> Domain.Value -> M Bool
uninhabitedScrutinee context coveredConstructors value = do
  value' <- Context.forceHead context value
  case value' of
    Domain.Neutral (Domain.Var var) spine -> do
      let
        varType =
          Context.lookupVarType var context
      type_ <- Context.instantiateType context varType $ toList spine
      uninhabitedType context 1 (IntMap.lookupDefault mempty var coveredConstructors) type_

    Domain.Neutral (Domain.Con constr) spine -> do
      constrType <- fetch $ Query.ConstructorType constr
      let
        args = snd <$> drop (Telescope.length constrType) (toList spine)
      anyM (uninhabitedScrutinee context coveredConstructors) args

    _ ->
      pure False

uninhabitedType
  :: Context v
  -> Int
  -> HashSet Name.QualifiedConstructor
  -> Domain.Type
  -> M Bool
uninhabitedType context fuel coveredConstructors type_ = do
  type' <- Context.forceHead context type_
  case type' of
    Builtin.Equals _ value1 value2 -> do
      result <- runExceptT $ Indices.unify context Flexibility.Rigid mempty value1 value2
      pure $ case result of
        Left Indices.Nope ->
          True

        Left Indices.Dunno ->
          False

        Right _ ->
          False

    Domain.Neutral (Domain.Global global) spine -> do
      maybeDefinitions <- fetch $ Query.ElaboratedDefinition global
      case maybeDefinitions of
        Just (Syntax.DataDefinition tele, _) -> do
          tele' <- Evaluation.evaluateConstructorDefinitions (Environment.empty $ Context.scopeKey context) tele
          tele'' <- Domain.Telescope.apply tele' $ toList spine
          case tele'' of
            Domain.Telescope.Empty constructors -> do
              let
                qualifiedConstructors =
                  HashMap.fromList
                    [ (Name.QualifiedConstructor global constr, constrType)
                    | (constr, constrType) <- HashMap.toList constructors
                    ]

                uncoveredConstructorTypes =
                  toList $
                  HashMap.difference qualifiedConstructors (HashSet.toMap coveredConstructors)

              allM (uninhabitedConstrType context fuel) uncoveredConstructorTypes

            _ ->
              pure False

        _ ->
          pure False

    _ ->
      pure False

uninhabitedConstrType :: Context v -> Int -> Domain.Type -> M Bool
uninhabitedConstrType context fuel type_ =
  case fuel of
    0 ->
      pure False

    _ -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi name source _ domainClosure -> do
          uninhabited <- uninhabitedType context (fuel - 1) mempty source
          if uninhabited then
            pure True

          else do
            (context', var) <- Context.extendUnnamed context name source
            domain <- Evaluation.evaluateClosure domainClosure $ Domain.var var
            uninhabitedConstrType context' fuel domain

        Domain.Fun source domain -> do
          uninhabited <- uninhabitedType context (fuel - 1) mempty source
          if uninhabited then
            pure True

          else
            uninhabitedConstrType context fuel domain

        _ ->
          pure False
