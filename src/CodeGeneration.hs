{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module CodeGeneration where

import Protolude hiding (IntMap, typeOf)

import Rock

import qualified Assembly
import Boxity
import ClosureConverted.Context (Context)
import qualified ClosureConverted.Context as Context
import qualified ClosureConverted.Evaluation as Evaluation
import qualified ClosureConverted.Readback as Readback
import qualified ClosureConverted.Syntax as Syntax
import qualified ClosureConverted.TypeOf as TypeOf
import qualified Core.Syntax
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Index
import qualified Literal
import Monad
import Name (Name)
import qualified Name
import Query (Query)
import qualified Query
import qualified Scope
import Telescope (Telescope)
import qualified Telescope
import Var (Var(Var))

newtype Builder a = Builder (StateT BuilderState M a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFetch Query, MonadState BuilderState)

newtype BuilderState = BuilderState
  { _instructions :: Tsil (Assembly.Instruction Assembly.BasicBlock)
  }

runBuilder :: Builder a -> M (a, Assembly.BasicBlock)
runBuilder (Builder s) =
  second (Assembly.BasicBlock . toList . _instructions) <$> runStateT s BuilderState
    { _instructions = mempty
    }

emit :: Assembly.Instruction Assembly.BasicBlock -> Builder ()
emit instruction =
  modify $ \s -> s { _instructions = _instructions s Tsil.:> instruction }

-------------------------------------------------------------------------------

data Environment v = Environment
  { _context :: Context v
  , _varLocations :: IntMap Var Assembly.Operand
  }

emptyEnvironment :: Scope.KeyedName -> Environment Void
emptyEnvironment scopeKey =
  Environment
    { _context = Context.empty scopeKey
    , _varLocations = mempty
    }

extend :: Environment v -> Syntax.Type v -> Assembly.Operand -> Builder (Environment (Succ v))
extend env type_ location =
  Builder $ lift $ do
    type' <- Evaluation.evaluate (Context.toEnvironment $ _context env) type_
    (context', var) <- Context.extend (_context env) type'
    pure Environment
      { _context = context'
      , _varLocations = IntMap.insert var location $ _varLocations env
      }

data Return = Return
  { _returnLocation :: !Assembly.Operand
  , _returnType :: !Assembly.Operand
  }

-------------------------------------------------------------------------------

indexLocation :: Index v -> Environment v -> Assembly.Operand
indexLocation index env = do
  let
    var =
      Context.lookupIndexVar index $ _context env
  fromMaybe (panic "CodeGeneration.indexLocation") $
    IntMap.lookup var $ _varLocations env

globalLocation :: Name.Lifted -> Assembly.Operand
globalLocation name =
  Assembly.Global $ Assembly.Name name 0

stackAllocate :: Assembly.Operand -> Builder Assembly.Operand
stackAllocate size = do
  return_ <- freshLocal
  emit $ Assembly.StackAllocate return_ size
  pure $ Assembly.LocalOperand return_

stackDeallocate :: Assembly.Operand -> Builder ()
stackDeallocate size =
  emit $ Assembly.StackDeallocate size

heapAllocate :: Assembly.Operand -> Builder Assembly.Operand
heapAllocate size = do
  return_ <- freshLocal
  emit $ Assembly.HeapAllocate return_ size
  pure $ Assembly.LocalOperand return_

typeOf :: Environment v -> Syntax.Term v -> Builder (Assembly.Operand, Builder ())
typeOf env term = do
  type_ <- Builder $ lift $ do
    value <- Evaluation.evaluate (Context.toEnvironment $ _context env) term
    typeValue <- TypeOf.typeOf (_context env) value
    Readback.readback (Context.toEnvironment $ _context env) typeValue
  generateTypedTerm env type_ pointerBytesOperand

sizeOfType :: Assembly.Operand -> Builder Assembly.Operand
sizeOfType =
  load

-------------------------------------------------------------------------------

freshLocal :: Builder Assembly.Local
freshLocal = do
  Var i <- Builder $ lift freshVar
  pure $ Assembly.Local i

copy :: Assembly.Operand -> Assembly.Operand -> Assembly.Operand -> Builder ()
copy destination source size =
  emit $ Assembly.Copy destination source size

call :: Name.Lifted -> [Assembly.Operand] -> Assembly.Operand -> Builder ()
call global args returnLocation =
  emit $ Assembly.CallVoid (Assembly.Global $ Assembly.Name global 0) (returnLocation : args)

load :: Assembly.Operand -> Builder Assembly.Operand
load pointer = do
  destination <- freshLocal
  emit $ Assembly.Load destination pointer
  pure $ Assembly.LocalOperand destination

store :: Assembly.Operand -> Assembly.Operand -> Builder ()
store destination int =
  emit $ Assembly.Store destination int

add :: Assembly.Operand -> Assembly.Operand -> Builder Assembly.Operand
add i1 i2 = do
  destination <- freshLocal
  emit $ Assembly.Add destination i1 i2
  pure $ Assembly.LocalOperand destination

sub :: Assembly.Operand -> Assembly.Operand -> Builder Assembly.Operand
sub i1 i2 = do
  destination <- freshLocal
  emit $ Assembly.Sub destination i1 i2
  pure $ Assembly.LocalOperand destination

-------------------------------------------------------------------------------

pointerBytes :: Num a => a
pointerBytes =
  8

pointerBytesOperand :: Assembly.Operand
pointerBytesOperand =
  Assembly.Lit $ Literal.Integer pointerBytes

-------------------------------------------------------------------------------

generateDefinition :: Name.Lifted -> Syntax.Definition -> M (Maybe (Assembly.Definition Assembly.BasicBlock))
generateDefinition name@(Name.Lifted qualifiedName _) definition = do
  case definition of
    Syntax.TypeDeclaration _ ->
      pure Nothing

    Syntax.ConstantDefinition term -> do
      ((), instructions) <- runBuilder $ do
        let
          env =
            emptyEnvironment $ Scope.KeyedName Scope.Definition qualifiedName
        (type_, deallocateType) <- typeOf env term
        typeSize <- sizeOfType type_
        termLocation <- heapAllocate typeSize
        storeTerm env term Return
          { _returnLocation = termLocation
          , _returnType = type_
          }
        deallocateType
        store (globalLocation name) termLocation
      pure $ Just $ Assembly.ConstantDefinition instructions

    Syntax.FunctionDefinition tele -> do
      (args, instructions) <- runBuilder $ do
        returnLocation <- freshLocal
        let
          env =
            emptyEnvironment $ Scope.KeyedName Scope.Definition qualifiedName
        args <- generateFunction env returnLocation tele
        return $ returnLocation : args
      pure $ Just $ Assembly.FunctionDefinition args instructions

    Syntax.DataDefinition constrDefs ->
      panic "gd dd"

    Syntax.ParameterisedDataDefinition tele ->
      panic "gd pd"

generateFunction
  :: Environment v
  -> Assembly.Local
  -> Telescope Name Syntax.Type Syntax.Term v
  -> Builder [Assembly.Local]
generateFunction env returnLocation tele =
  case tele of
    Telescope.Empty term -> do
      (type_, deallocateType) <- typeOf env term
      let
        return_ =
          Return
            { _returnLocation = Assembly.LocalOperand returnLocation
            , _returnType = type_
            }
      storeTerm env term return_
      deallocateType
      pure []

    Telescope.Extend _name type_ _plicity tele' -> do
      termLocation <- freshLocal
      env' <- extend env type_ $ Assembly.LocalOperand termLocation
      args <- generateFunction env' returnLocation tele'
      pure $ termLocation : args

-------------------------------------------------------------------------------

generateTerm :: Environment v -> Syntax.Term v -> Builder (Return, Builder ())
generateTerm env term = do
  (type_, deallocateType) <- typeOf env term
  (termLocation, deallocateTerm) <- generateTypedTerm env term type_
  let
    return_ =
      Return
        { _returnLocation = termLocation
        , _returnType = type_
        }
  pure (return_, deallocateTerm >> deallocateType)

generateTypedTerm :: Environment v -> Syntax.Term v -> Assembly.Operand -> Builder (Assembly.Operand, Builder ())
generateTypedTerm env term type_ = do
  let
    stackAllocateIt = do
      typeSize <- sizeOfType type_
      termLocation <- stackAllocate typeSize
      let
        return_ =
          Return
            { _returnLocation = termLocation
            , _returnType = type_
            }
      storeTerm env term return_
      pure (termLocation, stackDeallocate typeSize)

  case term of
    Syntax.Var index ->
      pure (indexLocation index env, pure ())

    Syntax.Global global ->
      pure (globalLocation global, pure ())

    Syntax.Con {} ->
      stackAllocateIt

    Syntax.Lit {} ->
      stackAllocateIt

    Syntax.Let _name term' termType body -> do
      (termType', deallocateType) <- generateTypedTerm env termType pointerBytesOperand
      (term'', deallocateTerm) <- generateTypedTerm env term' termType'
      env' <- extend env termType term''
      (result, deallocateBody) <- generateTypedTerm env' body type_
      deallocateTerm
      deallocateType
      pure (result, deallocateBody >> deallocateTerm >> deallocateType)

    Syntax.Function _ ->
      stackAllocateIt

    Syntax.Apply {} ->
      stackAllocateIt

    Syntax.Pi {} ->
      stackAllocateIt

    Syntax.Closure {} ->
      stackAllocateIt

    Syntax.ApplyClosure {} ->
      stackAllocateIt

    Syntax.Case {} ->
      stackAllocateIt

storeTerm
  :: Environment v
  -> Syntax.Term v
  -> Return
  -> Builder ()
storeTerm env term return_ =
  case term of
    Syntax.Var index -> do
      let
        varLocation =
          indexLocation index env
      returnTypeSize <- sizeOfType $ _returnType return_
      copy (_returnLocation return_) varLocation returnTypeSize

    Syntax.Global global -> do
      let
        location =
          globalLocation global
      returnTypeSize <- sizeOfType $ _returnType return_
      copy (_returnLocation return_) location returnTypeSize

    Syntax.Con con@(Name.QualifiedConstructor typeName _) params args -> do
      maybeTag <- fetch $ Query.ConstructorTag con
      let
        tagArgs =
          case maybeTag of
            Nothing ->
              args

            Just tag ->
              Syntax.Lit (Literal.Integer $ fromIntegral tag) : args

        go location arg = do
          (argType, deallocateArgType) <- typeOf env arg
          storeTerm env arg Return
            { _returnType = argType
            , _returnLocation = location
            }
          deallocateArgType
          argTypeSize <- sizeOfType argType
          add location argTypeSize

      boxity <- fetchBoxity typeName
      case boxity of
        Unboxed ->
          foldM_ go (_returnLocation return_) tagArgs

        Boxed -> do
          size <- boxedConstructorSize env con params
          heapLocation <- heapAllocate size
          foldM_ go heapLocation tagArgs
          store (_returnLocation return_) heapLocation

    Syntax.Lit lit ->
      store (_returnLocation return_) (Assembly.Lit lit)

    Syntax.Let _name term' type_ body -> do
      (type', deallocateType) <- generateTypedTerm env type_ pointerBytesOperand
      (term'', deallocateTerm) <- generateTypedTerm env term' type'
      env' <- extend env type_ term''
      storeTerm env' body return_
      deallocateTerm
      deallocateType

    Syntax.Function _ ->
      store (_returnLocation return_) pointerBytesOperand

    Syntax.Apply global args -> do
      (args', deallocators) <- unzip <$> forM args (generateTerm env)
      call global (_returnLocation <$> args') $ _returnLocation return_
      sequence_ $ reverse deallocators

    Syntax.Pi {} ->
      store (_returnLocation return_) pointerBytesOperand

    Syntax.Closure global args ->
      panic "st c"

    Syntax.ApplyClosure operand args ->
      panic "st ac"

    Syntax.Case scrutinee branches defaultBranch ->
      panic "st case"

-------------------------------------------------------------------------------

fetchBoxity :: MonadFetch Query m => Name.Qualified -> m Boxity
fetchBoxity name = do
  maybeDef <- fetch $ Query.ElaboratedDefinition name
  case maybeDef of
    Just (Core.Syntax.DataDefinition boxity _, _) ->
      pure boxity

    _ ->
      panic "CodeGeneration.fetchBoxity"

boxedConstructorSize
  :: Environment v
  -> Name.QualifiedConstructor
  -> [Syntax.Term v]
  -> m Assembly.Operand
boxedConstructorSize =
  panic "bcs"
