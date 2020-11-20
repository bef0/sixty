{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module CodeGeneration where

import Protolude hiding (IntMap, typeOf)

import Data.Coerce (coerce)
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

data BuilderState = BuilderState
  { _instructions :: Tsil (Assembly.Instruction Assembly.BasicBlock)
  , _stackPointer :: !PointerOperand
  }

runBuilder :: PointerOperand -> Builder a -> M (a, Assembly.BasicBlock)
runBuilder stackPointer (Builder s) =
  second (Assembly.BasicBlock . toList . _instructions) <$> runStateT s BuilderState
    { _instructions = mempty
    , _stackPointer = stackPointer
    }

emit :: Assembly.Instruction Assembly.BasicBlock -> Builder ()
emit instruction =
  modify $ \s -> s { _instructions = _instructions s Tsil.:> instruction }

-------------------------------------------------------------------------------

newtype StackPointerOperand = StackPointerOperand Assembly.Operand
newtype PointerOperand = PointerOperand Assembly.Operand
newtype IntOperand = IntOperand Assembly.Operand

data Environment v = Environment
  { _context :: Context v
  , _varLocations :: IntMap Var PointerOperand
  }

emptyEnvironment :: Scope.KeyedName -> Environment Void
emptyEnvironment scopeKey =
  Environment
    { _context = Context.empty scopeKey
    , _varLocations = mempty
    }

extend :: Environment v -> Syntax.Type v -> PointerOperand -> Builder (Environment (Succ v))
extend env type_ location =
  Builder $ lift $ do
    type' <- Evaluation.evaluate (Context.toEnvironment $ _context env) type_
    (context', var) <- Context.extend (_context env) type'
    pure Environment
      { _context = context'
      , _varLocations = IntMap.insert var location $ _varLocations env
      }

data Return = Return
  { _returnLocation :: !PointerOperand
  , _returnTypeSize :: !IntOperand
  }

-------------------------------------------------------------------------------

indexLocation :: Index v -> Environment v -> PointerOperand
indexLocation index env = do
  let
    var =
      Context.lookupIndexVar index $ _context env
  fromMaybe (panic "CodeGeneration.indexLocation") $
    IntMap.lookup var $ _varLocations env

globalLocation :: Name.Lifted -> PointerOperand
globalLocation name =
  PointerOperand $ Assembly.Global $ Assembly.Name name 0

stackAllocate :: IntOperand -> Builder PointerOperand
stackAllocate size = do
  stackPointer <- gets _stackPointer
  stackPointerInt <- pointerToInt stackPointer
  newStackPointerInt <- sub stackPointerInt size
  newStackPointer <- intToPointer newStackPointerInt
  modify $ \s -> s
    { _stackPointer = newStackPointer
    }
  pure newStackPointer

stackDeallocate :: IntOperand -> Builder ()
stackDeallocate size = do
  stackPointer <- gets _stackPointer
  stackPointerInt <- pointerToInt stackPointer
  newStackPointerInt <- add stackPointerInt size
  newStackPointer <- intToPointer newStackPointerInt
  modify $ \s -> s
    { _stackPointer = newStackPointer
    }

heapAllocate :: IntOperand -> Builder PointerOperand
heapAllocate (IntOperand size) = do
  return_ <- freshLocal
  emit $ Assembly.HeapAllocate return_ size
  pure $ PointerOperand $ Assembly.LocalOperand return_

typeOf :: Environment v -> Syntax.Term v -> Builder PointerOperand
typeOf env term = do
  type_ <- Builder $ lift $ do
    value <- Evaluation.evaluate (Context.toEnvironment $ _context env) term
    putText $ "typeOf " <> show term
    typeValue <- TypeOf.typeOf (_context env) value
    putText "typeOf done"
    result <- Readback.readback (Context.toEnvironment $ _context env) typeValue
    putText "typeOf rb"
    pure result
  typeLocation <- stackAllocate pointerBytesOperand
  storeTerm env type_ Return
    { _returnLocation = typeLocation
    , _returnTypeSize = pointerBytesOperand
    }
  pure typeLocation

sizeOfType :: PointerOperand -> Builder IntOperand
sizeOfType =
  load

-------------------------------------------------------------------------------

freshLocal :: Builder Assembly.Local
freshLocal = do
  Var i <- Builder $ lift freshVar
  pure $ Assembly.Local i

copy :: PointerOperand -> PointerOperand -> IntOperand -> Builder ()
copy (PointerOperand destination) (PointerOperand source) (IntOperand size) =
  emit $ Assembly.Copy destination source size

call :: Name.Lifted -> [PointerOperand] -> PointerOperand -> Builder ()
call global args returnLocation =
  emit $ Assembly.CallVoid (Assembly.Global $ Assembly.Name global 0) (coerce $ returnLocation : args)

load :: PointerOperand -> Builder IntOperand
load (PointerOperand pointer) = do
  destination <- freshLocal
  emit $ Assembly.Load destination pointer
  pure $ IntOperand $ Assembly.LocalOperand destination

store :: PointerOperand -> IntOperand -> Builder ()
store (PointerOperand destination) (IntOperand int) =
  emit $ Assembly.Store destination int

add :: IntOperand -> IntOperand -> Builder IntOperand
add (IntOperand i1) (IntOperand i2) = do
  destination <- freshLocal
  emit $ Assembly.Add destination i1 i2
  pure $ IntOperand $ Assembly.LocalOperand destination

sub :: IntOperand -> IntOperand -> Builder IntOperand
sub (IntOperand i1) (IntOperand i2) = do
  destination <- freshLocal
  emit $ Assembly.Sub destination i1 i2
  pure $ IntOperand $ Assembly.LocalOperand destination

pointerToInt :: PointerOperand -> Builder IntOperand
pointerToInt (PointerOperand pointer) = do
  destination <- freshLocal
  emit $ Assembly.PointerToInt destination pointer
  pure $ IntOperand $ Assembly.LocalOperand destination

intToPointer :: IntOperand -> Builder PointerOperand
intToPointer (IntOperand integer) = do
  destination <- freshLocal
  emit $ Assembly.IntToPointer destination integer
  pure $ PointerOperand $ Assembly.LocalOperand destination

-------------------------------------------------------------------------------

pointerBytes :: Num a => a
pointerBytes =
  8

pointerBytesOperand :: IntOperand
pointerBytesOperand =
  IntOperand $ Assembly.Lit $ Literal.Integer pointerBytes

-------------------------------------------------------------------------------

generateDefinition :: Name.Lifted -> Syntax.Definition -> M (Maybe (Assembly.Definition Assembly.BasicBlock))
generateDefinition name@(Name.Lifted qualifiedName _) definition = do
  Var i <- freshVar
  let
    stackPointerLocal
      = Assembly.Local i
    stackPointer =
      PointerOperand $ Assembly.LocalOperand stackPointerLocal
  case definition of
    Syntax.TypeDeclaration _ ->
      pure Nothing

    Syntax.ConstantDefinition term -> do
      ((), instructions) <- runBuilder stackPointer $ do
        let
          env =
            emptyEnvironment $ Scope.KeyedName Scope.Definition qualifiedName
        type_ <- typeOf env term
        typeSize <- sizeOfType type_
        termLocation <- heapAllocate typeSize
        storeTerm env term Return
          { _returnLocation = termLocation
          , _returnTypeSize = typeSize
          }
        termLocationInt <- pointerToInt termLocation
        store (globalLocation name) termLocationInt
      pure $ Just $ Assembly.ConstantDefinition stackPointerLocal instructions

    Syntax.FunctionDefinition tele -> do
      (args, instructions) <- runBuilder stackPointer $ do
        returnLocation <- freshLocal
        let
          env =
            emptyEnvironment $ Scope.KeyedName Scope.Definition qualifiedName
        args <- generateFunction env returnLocation tele
        return $ returnLocation : args
      pure $ Just $ Assembly.FunctionDefinition stackPointerLocal args instructions

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
      putText $ "gf typeOf " <> show term
      type_ <- typeOf env term
      typeSize <- sizeOfType type_
      let
        return_ =
          Return
            { _returnLocation = PointerOperand $ Assembly.LocalOperand returnLocation
            , _returnTypeSize = typeSize
            }
      storeTerm env term return_
      pure []

    Telescope.Extend _name type_ _plicity tele' -> do
      termLocation <- freshLocal
      env' <- extend env type_ $ PointerOperand $ Assembly.LocalOperand termLocation
      args <- generateFunction env' returnLocation tele'
      pure $ termLocation : args

-------------------------------------------------------------------------------

generateTerm :: Environment v -> Syntax.Term v -> Builder Return
generateTerm env term = do
  putText $ "gt typeOf " <> show term
  type_ <- typeOf env term
  typeSize <- sizeOfType type_
  termLocation <- stackAllocate typeSize
  let
    return_ =
      Return
        { _returnLocation = termLocation
        , _returnTypeSize = typeSize
        }
  storeTerm env term return_
  pure return_

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
      copy (_returnLocation return_) varLocation (_returnTypeSize return_)

    Syntax.Global global -> do
      let
        location =
          globalLocation global
      copy (_returnLocation return_) location (_returnTypeSize return_)

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
          argReturn <- generateTerm env arg
          putText $ "gt typeOf con " <> show arg
          argType <- typeOf env arg
          argTypeSize <- sizeOfType argType
          storeTerm env arg Return
            { _returnTypeSize = argTypeSize
            , _returnLocation = location
            }
          locationInt <- pointerToInt location
          newLocationInt <- add locationInt argTypeSize
          intToPointer newLocationInt

      boxity <- fetchBoxity typeName
      case boxity of
        Unboxed ->
          foldM_ go (_returnLocation return_) tagArgs

        Boxed -> do
          size <- boxedConstructorSize env con params
          heapLocation <- heapAllocate size
          heapLocationInt <- pointerToInt heapLocation
          foldM_ go heapLocation tagArgs
          store (_returnLocation return_) heapLocationInt

    Syntax.Lit lit ->
      store (_returnLocation return_) (IntOperand $ Assembly.Lit lit)

    Syntax.Let _name term' type_ body -> do
      typeLocation <- stackAllocate pointerBytesOperand
      storeTerm env type_ Return
        { _returnLocation = typeLocation
        , _returnTypeSize = pointerBytesOperand
        }
      typeSize <- sizeOfType typeLocation
      termLocation <- stackAllocate typeSize
      storeTerm env term' Return
        { _returnLocation = termLocation
        , _returnTypeSize = typeSize
        }
      env' <- extend env type_ termLocation
      storeTerm env' body return_
      stackDeallocate typeSize
      stackDeallocate pointerBytesOperand

    Syntax.Function _ ->
      store (_returnLocation return_) pointerBytesOperand

    Syntax.Apply global args -> do
      args' <- forM args $ \arg -> do
        putText $ "gt typeOf apply " <> show arg
        type_ <- typeOf env arg
        typeSize <- sizeOfType type_
        argLocation <- stackAllocate typeSize
        storeTerm env arg Return
          { _returnLocation = argLocation
          , _returnTypeSize = typeSize
          }
        pure (argLocation, typeSize)
      call global (fst <$> args') $ _returnLocation return_
      forM_ args' $ stackDeallocate . snd

    Syntax.Pi _ _ _ ->
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
  -> m IntOperand
boxedConstructorSize =
  panic "bcs"
